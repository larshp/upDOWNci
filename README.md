# upDOWNci
Upload and download [SAP Code Inspector](http://wiki.scn.sap.com/wiki/display/ABAP/Code+Inspector) variants in XML format

- Version requirement: 702
- MIT License 


## Installation
Install via [abapGit](https://github.com/larshp/abapGit).

## Use

### Download and upload single variants

You can utilize program `ZUPDOWNCI` to download and upload single check variants.

Leave username blank for global variants.

![selection](http://larshp.github.io/upDOWNci/img/selection.png)

Upload will create or add checks to the specified variant.

![upload](http://larshp.github.io/upDOWNci/img/upload.png)

### Mass-download of variants

You can utilize program `ZUPDOWNCI_MASS_DOWNLOAD` to download multiple variants as a single ZIP file.
User-specific variants are stored in a folder named after the user.

If the download fails due to a specific check variant, exclude it on the selection screen.
