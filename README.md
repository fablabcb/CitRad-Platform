# CitRad Sensor Platform


## versatile map

- [versatiles.org](https://versatiles.org)

The map is configured in `www/neutrino.de.json`. In R it is included with the `mapgl` package with the `renderMaplibre()` function

### fonts

Fonts come from [versatile-fonts](https://github.com/versatiles-org/versatiles-fonts)

```
git clone git@github.com:versatiles-org/versatiles-fonts.git
cd versatile-fonts
npm install
npm run build
```

This will create:

- A `dist/fonts.tar.gz` archive containing all glyphs.
- Separate archives `dist/{font_family}.tar.gz` for each individual font family.

### sprites

#### load without building

Sprites can be downloaded from [here](https://github.com/versatiles-org/versatiles-style/releases/latest/download/sprites.tar.gz) and unpacked into the www folder.

#### build sprites

Alle Informationen hier: https://github.com/versatiles-org/versatiles-style

```
git clone https://github.com/versatiles-org/versatiles-style
cd versatile-style
npm install optipng
npm run build-sprites
```

Sprites will be generated in `versatiles-style/release/sprites/`

Für eigene Icons SVG Datei in `icons/icon` anlegen oder eines der existierenden bearbeiten. 

Berücksichtigen:

- SVGs must consist only of paths and should not contain any transform() attributes.
- Styles and colors within the SVG are ignored.
- All length values must be specified in pixels without units.

Mit der [applytransforms Erweiterung](https://github.com/Klowner/inkscape-applytransforms) für Inkscape kann man alle "transform()" Attribute in feste Koordinaten umwandeln. Wichtig: vorher alle Gruppierungen aufheben.

