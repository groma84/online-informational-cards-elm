module.exports = {
    configureWebpack: (config, env) => {
        // set watching to "poll" to make it work in Windows 10 Docker
        console.log(config);

        config.watch = true;
        config.watchOptions = {
            aggregateTimeout: 500,
            poll: 2000
        };
        return config;
    }
}