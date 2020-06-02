# Capacity planner
A small project to help plan for projects with a given capacity.

## Running
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
$ clj -m capacity.runner my-config.edn
```
