"""Generic wrapper for read-eval-print-loops, a.k.a. interactive shells
"""
import os.path
import signal
import sys

import pexpect

PY3 = (sys.version_info[0] >= 3)

if PY3:
    basestring = str

PEXPECT_PROMPT = u'[PEXPECT_PROMPT>'
PEXPECT_CONTINUATION_PROMPT = u'[PEXPECT_PROMPT+'

class REPLWrapper(object):
    """Wrapper for a REPL.

    :param cmd_or_spawn: This can either be an instance of :class:`pexpect.spawn`
      in which a REPL has already been started, or a str command to start a new
      REPL process.
    :param str orig_prompt: The prompt to expect at first.
    :param str prompt_change: A command to change the prompt to something more
      unique. If this is ``None``, the prompt will not be changed. This will
      be formatted with the new and continuation prompts as positional
      parameters, so you can use ``{}`` style formatting to insert them into
      the command.
    :param str new_prompt: The more unique prompt to expect after the change.
    :param str extra_init_cmd: Commands to do extra initialisation, such as
      disabling pagers.
    """
    def __init__(self, cmd_or_spawn, orig_prompt, extra_init_cmd=None):
        if isinstance(cmd_or_spawn, basestring):
            self.child = pexpect.spawn(cmd_or_spawn, echo=False)
        else:
            self.child = cmd_or_spawn
        if self.child.echo:
            # Existing spawn instance has echo enabled, disable it
            # to prevent our input from being repeated to output.
            self.child.setecho(False)
            self.child.waitnoecho()

        self.prompt = orig_prompt
        self._expect_prompt()

        if extra_init_cmd is not None:
            self.run_command(extra_init_cmd)

    def set_prompt(self, orig_prompt):
        self.child.expect(orig_prompt)

    def _expect_prompt(self, timeout=-1, async_=False):
        return self.child.expect([self.prompt], timeout=timeout)

    def run_command(self, command, timeout=-1, async_=False):
        """Send a command to the REPL, wait for and return output.

        :param str command: The command to send. Trailing newlines are not needed.
          This should be a complete block of input that will trigger execution;
          if a continuation prompt is found after sending input, :exc:`ValueError`
          will be raised.
        :param int timeout: How long to wait for the next prompt. -1 means the
          default from the :class:`pexpect.spawn` object (default 30 seconds).
          None means to wait indefinitely.
        :param bool async_: On Python 3.4, or Python 3.3 with asyncio
          installed, passing ``async_=True`` will make this return an
          :mod:`asyncio` Future, which you can yield from to get the same
          result that this method would normally give directly.
        """
        # Split up multiline commands and feed them in bit-by-bit
        print("interpreter running:", command)
        cmdlines = command.splitlines()
        # splitlines ignores trailing newlines - add it back in manually
        if command.endswith(b'\n'):
            cmdlines.append(b'')
        if not cmdlines:
            raise ValueError("No command was given")

        if async_:
            from ._async import repl_run_command_async
            return repl_run_command_async(self, cmdlines, timeout)

        res = []
        self.child.sendline(cmdlines[0])
        for line in cmdlines[1:]:
            self._expect_prompt(timeout=timeout)
            res.append(self.child.before.decode())
            self.child.sendline(line)

        # Command was fully submitted, now wait for the next prompt
        if self._expect_prompt(timeout=timeout) == 1:
            # We got the continuation prompt - command was incomplete
            self.child.kill(signal.SIGINT)
            self._expect_prompt(timeout=1)
            raise ValueError("Continuation prompt found - input was incomplete:\n"
                             + command)

        print(self.child.before.decode())
        print(self.child.after.decode())
        return u''.join(res + [self.child.before.decode()])


def ghci(command="stack ghci InternalGenType.hs"):
    """Start a Python shell and return a :class:`REPLWrapper` object."""
    repl = REPLWrapper(command, u">")
    repl.run_command("import Test.SmallCheck\n".encode())
    repl.run_command("import Test.SmallCheck.Drivers\n".encode())
    repl.run_command("import Data.Maybe\n".encode())
    repl.run_command(":set -XScopedTypeVariables\n".encode())
    repl.run_command(":set -XFlexibleContexts\n".encode())
    return repl