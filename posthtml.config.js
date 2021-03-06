const packageJson = require("./package.json");

const root = "benchmark/gh-pages";

const info = {
    url: `${process.env.GAME_URL}`,
    title: "Elm Math Benchmark",
    description: "This is alternative to original Elm linear-algebra and can NOT be used directly with WebGL, but is useful for anything where fast linear algebra is needed.",
    version: packageJson.version,
    license: packageJson.license,
    twitterName: "@justgook",
    image: "image.jpg",

};
const socialTags_ = ({ facebook, twitter }) =>
    ({
        "meta": [
            // facebook
            { "property": "og:type", "content": facebook.type },
            // { "property": "og:url", "content": facebook.url },
            { "property": "og:title", "content": facebook.title },
            { "property": "og:description", "content": facebook.description },
            { "property": "og:image", "content": facebook.image },
            { "property": "og:image:width", "content": facebook.image_width },
            { "property": "og:image:height", "content": facebook.image_height },
            // twitter
            { "name": "twitter:card", "content": twitter.card },
            // { "name": "twitter:domain", "content": twitter.domain },
            { "name": "twitter:title", "content": twitter.title },
            { "name": "twitter:description", "content": twitter.description },
            { "name": "twitter:image", "content": twitter.image },
            // { "name": "twitter:url", "content": twitter.url },
            // { "name": "twitter:label1", "content": twitter.label1 },
            // { "name": "twitter:data1", "content": twitter.data1 },
            // { "name": "twitter:label2", "content": twitter.label2 },
            // { "name": "twitter:data2", "content": twitter.data2 },
            { "name": "twitter:site", "content": twitter.name },
            { "name": "twitter:creator", "content": twitter.name },
        ]
    });

const buildSocialTags = ({ url, image, title, description, version, license, twitterName, facebookId }) => socialTags_({
    facebook: {
        type: "website",
        url,
        title,
        description,
        image: url + image,
        image_width: 1200,
        image_height: 675,
        facebookId
    },
    twitter: {
        card: "summary_large_image",
        domain: "z0.lv",
        title,
        description,
        image: url + image,
        url,
        label1: "Version",
        data1: version,
        label2: "License",
        data2: license,
        name: twitterName
    }
});


module.exports = {
    plugins: {
        // "posthtml-content": {
        //     start: (str) => str.replace(/#GAME_URL#/g, `${process.env.GAME}.age.bin`)
        // },
        "posthtml-style-to-file": {
            path: `${root}/app.css`,
            removeStyle: "all",
        },
        "posthtml-head-elements": {
            headElements:
                {
                    "meta": [
                        {
                            "charset": "utf-8"
                        },

                        {
                            "http-equiv": "X-UA-Compatible",
                            "content": "IE=edge"
                        },
                        {
                            "name": "description",
                            "content": info.description
                        },
                        {
                            "name": "viewport",
                            "content": "width=device-width, initial-scale=1"
                        }
                    ].concat(buildSocialTags(info).meta),
                    "title": `${process.env.GAME} (${info.title})`,

                    "base": [
                        {
                            "href": "/"
                        }
                    ],
                    "link": [
                        { "rel": "icon", "href": info.favicon },
                        { "rel": "stylesheet", href: "app.css" }
                    ],

                    "script": [
                        { "src": `bundle.min.js` }
                    ]
                }

        },
        "htmlnano": {}
    }

};
