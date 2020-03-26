# RCMEMS
RCMEMS provides an R interface to the Copernicus Marine Environment Monitoring Service (CMEMS) by wrapping around the more-standard Python-based Motu Client.

## Quick start

### Dependencies
RCMEMS acts as an R wrapper around the "motuclient" developed by CLS in Tolouse, France. You will also need to download and install this client by following the instructions here:

https://github.com/clstoulouse/motu-client-python

RCMEMS has been rewritten to reflect changes in version 1.8.4 of the client. It is no-longer backwards compatible with previous versions.

Motuclient is written in the Python language, which you will also need to install if you don't have it already. You can get Python from here:

https://www.python.org/

As a general rule, it is simplest to install the motuclient module in python (i.e. using the "pip" command).

Remember that you will also need to provide the details of your user-account on CMEMS, so make sure you have that handy too. Sign up here: 

http://marine.copernicus.eu/services-portfolio/register-now/

### Installing RCMEMS

You can install this package directly from the GitHub repository using the following command:

```{R}
devtools::install_github("markpayneatwork/RCMEMS")
```
### Passwords

Including usernames and passwords in scripts is generally not a particularly good idea, especially if you are also publishing the code on e.g. GitHub. The Motu-client provides a way around this, by letting you specify the passwords in a separate file on your machine. See the motu documentation for more details:

https://github.com/clstoulouse/motu-client-python#Configuration


## RCMEMS Workflow

Once installed, using RCMEMS involves the following steps

* Setup an RCMEMS object with all of the necessary configuration options. This can be done in one of two different ways
    + Manually, setting the options individually using `CMEMS.config()`
    + Based on a download script provided by the CMEMS website, using the `parse.CMEMS.script()` function.
* Modify the configuration object further, if required, using the `update()` function
* When you're ready to go, download the data using the `CMEMS.download()`function.

## Getting help

The package is (reasonably) well documented, so check the help files first. The help documentation for the motu-client can also be useful at times. If all else fails after reasonable efforts, please feel free to file a bug-report here (under "Issues") - that way, everyone can see the problem and hopefully learn from it. I'm happy to offer help, but prefer it to go through the Issue tracker.

