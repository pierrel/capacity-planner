# Capacity planner
A small project to help plan for projects with a given capacity.

# Running
## CLI
### Config
The config file has a few parts: constants, proficiencies, contributions, and projects. The file should end in `.edn` and look like the following:
```
{:constants {:sprints   5
             :unplanned 0.2
             :velocity  4}
 :profs     {:bob       #{:app :ios}
             :kathy     #{:app}
             :jenny     #{:app :data}
             :dave      #{:web :app}
             :josephina #{:android :app}}
 :contrib   {:bob        0
             :kathy      0.4
             :jenny      1
             :dave       0.9
             :josephina  1}
 :projects  ({:name   "Athena"
              :effort {:app 32}}
             {:name   "Durian"
              :effort {:web 45}})}
```

### Runner
You can run it using the runner file:
```
$ ./scripts/run my-config.edn
```
## Web
*Still under development - stick to the CLI for now*
### Development
There are two main files for development: `utils/dev.clj` and `utils/repl.cljs`. I mainly use emacs and cider so most of the variables will be correctly set up in `.dir-locals.el`.

#### Running the server
If using emacs/cider just pop open the repl with the `cider-jack-in-clj` function. Then you can navigate to `utils/dev.clj` and load the code with `cider-load-buffer`. This will start the server so that you can navigate to http://localhost:3000/config (where "config" is a config file as described above). You can stop the server by killing the repl buffer or changing the repl ns to "dev" with `(in-ns 'dev)` and then `(.stop server)`.

Otherwise you have to open a repl in the "dev" alias (`-A:dev`) and load the `dev` namespace.

#### JS repl
If using emacs/cider, open a cljs repl with `cider-jack-in-cljs`. You may need to reload the web page.

Otherwise you're on your own!

### Production
There's no production-like way to run this but it'll probably be deployed over docker or something similar.
