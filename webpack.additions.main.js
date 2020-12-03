var path = require("path");

function resolve(filePath) {
    return path.join(__dirname, filePath)
}

module.exports = {
    entry: resolve("src/main/main.fs.js"),
    module: {
        rules: [

        ]
    }
}
