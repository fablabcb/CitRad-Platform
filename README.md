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

Sprites can be downloaded from [here](https://github.com/versatiles-org/versatiles-style/releases/latest/download/sprites.tar.gz) and unpacked into the www folder.

On how to build sprites: https://github.com/versatiles-org/versatiles-style
