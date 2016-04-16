# hrunr
## Synopsis

hrunr is a CLI tool to interact with [Rundeck](http://www.rundeck.org)'s API. It is by no means complete. In the current state, hrunr is more useful for triggering and viewing the output of running jobs than it is for manipulating job definitions, as that functionality has not been implemented.

The version of the API supported in the current release is v12. Please consult Rundeck's [API documentation](http://rundeck.org/2.4.2/api/index.html) for information related to specific API endpoints.

## Installing

hrunr is written in Haskell and can therefore most easily be built using stack. To do this obtain the latest release from https://github.com/commercialhaskell/stack/releases and build/install with
```bash
stack build --copy-bins
```

## Usage Example

Help can be obtained by running `hrunr --help`. hrunr is a multi-mode application with each mode relating to a particular grouping of API calls. Help for the individual modes can be obtained with `hrunr <mode> --help`.

Authentication is currently performed through tokens. This can be passed with the `--authtoken` switch.

To trigger a job, called for example 'bar' in the 'foo' group, and tail the output run:
```bash
hrunr runjob --name foo/bar --follow --authtoken xyz
```

Arguments to a job can be passed with `--argstring` switch. The form required is `--argstring "-opt value -opt2 value2"`.

## Motivation

At the time, Rundeck did not support triggering multiple jobs in parallel from a single job. The only way to do it was to call Rundeck's API from a script in the job. Rundeck provides an rd-job tool to do this but for various reasons the tool didn't quite do what I needed. As a result, an initial ruby version of this tool, called runr, was written to supply the needed functionality. Over time, though, it became a little difficult to maintain and refactoring to squeeze in functionality that was not in the original design scope became painful. hrunr was born out of this, and was a complete rewrite of runr in Haskell.

## Contributors

Feel free to contribute by sending pull requests.

## License

hrunr is licensed under a BSD-style license.
