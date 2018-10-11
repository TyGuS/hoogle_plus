package maybe;

public class Maybe<T> {
	private T content;
	public Maybe() {

	}
	
	public Maybe(T t) {
		this.content = t;
	}
	public T fromJust() { return content;}
}