### #[untagged] Enums

- **Status**: Not yet in compiler
- **Impact**: Complex unions generate JsValue instead of untagged enums
- **Workaround**: Use `union_strategy = "jsvalue"` explicitly
- **Tracking**: [issue link]

### extern "js" const

- **Status**: Not yet in compiler
- **Impact**: Module constants generate zero-arg functions
- **Workaround**: Call the function: `let url = API_URL();`
- **Tracking**: [issue link]

### Phantom Type Parameters

- **Status**: Not yet in compiler
- **Impact**: Generic types require manual monomorphization config
- **Workaround**: Specify concrete types in `husk.toml`
- **Tracking**: [issue link]

### this Binding

- **Status**: Not yet in compiler
- **Impact**: Methods with explicit `this` parameter degrade
- **Workaround**: None currently
- **Tracking**: [issue link]

```

```
