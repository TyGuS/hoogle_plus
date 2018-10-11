package cmu.edu.server;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import com.google.gson.Gson;

import cmu.edu.parser.SyPetInput;
import cmu.edu.ui.UISyPet;
import fi.iki.elonen.NanoHTTPD;
import fi.iki.elonen.NanoHTTPD.Response.Status;
import fi.iki.elonen.util.ServerRunner;

/**
 * A custom subclass of NanoHTTPD.
 */
public class HttpServer extends NanoHTTPD {

	private static final Logger LOG = Logger.getLogger(HttpServer.class.getName());
	private static int httpServerPort = 9092;
	private static HashMap<String, String> mappingLibs = new HashMap<String, String>();
	private static ArrayList<String> allLibs = new ArrayList<String>();
	private boolean sootInit = true;

	public static void main(String[] args) {
		// Start the HTTP Server
		mappingLibs.put("java.", "./lib/rt8.jar");
		mappingLibs.put("org.apache.commons.codec.", "./lib/org.apache.commons.codec-1.9.0.jar");
		mappingLibs.put("javax.crypto.", "./lib/javax-crypto.jar");
//		allLibs = new ArrayList<String>(Arrays.asList("./lib/rt8.jar", "./lib/org.apache.commons.codec-1.9.0.jar",
//				"./lib/javax-crypto.jar", "./lib/symail.jar", "./lib/greenmail-standalone-1.5.8.jar",
//				"./lib/javax.mail.jar", "./lib/simple-java-mail-5.0.3.jar", "./lib/emailaddress-rfc2822-1.1.0.jar"));
		allLibs = new ArrayList<String>(Arrays.asList("./lib/rt8.jar", "./lib/org.apache.commons.codec-1.9.0.jar",
				"./lib/javax-crypto.jar", "./lib/symail.jar"));
		ServerRunner.run(HttpServer.class);
	}

	public HttpServer() {
		super(httpServerPort);
		System.out.println("SyPet HttpServer starting on port: " + httpServerPort);
	}

	String run(String bench) {

		List<String> soot = new ArrayList<>();

		SyPetInput jsonInput = new Gson().fromJson(bench, SyPetInput.class);
		List<String> packages = jsonInput.packages;
		List<String> libs = new ArrayList<String>();
		libs.addAll(allLibs);
		if (sootInit) {
			soot.addAll(allLibs);
			sootInit = false;
		}

		List<String> hints = new ArrayList<String>();
		if (jsonInput.hints != null)
			hints = jsonInput.hints;

		if (hints.size() == 1 && hints.get(0).equals(""))
			hints.clear();

		UISyPet sypet = new UISyPet(packages, libs, soot, hints);
		String methodName = jsonInput.methodName;
		List<String> paramNames = jsonInput.paramNames;
		List<String> srcTypes = jsonInput.srcTypes;
		String tgtType = jsonInput.tgtType;
		String testCode = jsonInput.testBody;

//		// FIXME: hack for $ types
//		for (int p = 0; p < srcTypes.size(); p++) {
//			if (srcTypes.get(p).equals("javax.mail.Message.RecipientType"))
//				srcTypes.set(p, "javax.mail.Message$RecipientType");
//		}

		sypet.setSignature(methodName, paramNames, srcTypes, tgtType, testCode);
		int lb = 0;
		int ub = 10;
		if (jsonInput.lb != 0)
			lb = jsonInput.lb;

		if (jsonInput.ub != 0)
			ub = jsonInput.ub;

		String code = sypet.synthesize(lb, ub);
		if (!code.equals("")) {
			code = code.substring(code.indexOf("{"), code.indexOf("}"));
			code = code.replace("{", "");
			code = code.replace("}", "");
			return code;
		}

		// TODO: check if the test compiles with a empty method
		return "// SyPet failure!\n";
	}

	@Override
	public Response serve(IHTTPSession session) {

		String response = "";
		try {
			Method method = session.getMethod();
			String uri = session.getUri();
			HttpServer.LOG.info(method + " '" + uri + "' ");
			session.getHeaders();

			if (session.getMethod() == Method.POST) {

				Map<String, String> form = new HashMap<String, String>();
				session.parseBody(form);
				String benchmark = "";
				for (String s : form.keySet()) {
					benchmark += form.get(s);
				}
				response = run(benchmark);
				Response resp = newFixedLengthResponse(Status.OK, NanoHTTPD.MIME_PLAINTEXT, response);
				resp.addHeader("Access-Control-Allow-Origin", "*");
				resp.addHeader("Access-Control-Allow-Methods", "POST");
				return resp;

			}

		} catch (Exception e) {
			HttpServer.LOG.info("exception e = " + e);
			response = "// SyPet error: Http Server exception!\n";
			Response resp = newFixedLengthResponse(Status.OK, NanoHTTPD.MIME_PLAINTEXT, response);
			resp.addHeader("Access-Control-Allow-Origin", "*");
			resp.addHeader("Access-Control-Allow-Methods", "POST");
			return resp;
		}

		response += "// SyPet error: Http Server does not support the received request!\n";
		Response resp = newFixedLengthResponse(Status.OK, NanoHTTPD.MIME_PLAINTEXT, response);
		resp.addHeader("Access-Control-Allow-Origin", "*");
		resp.addHeader("Access-Control-Allow-Methods", "POST");
		return resp;
	}
}
