use anyhow::{Context, Result};
use std::path::Path;
use swc_common::sync::Lrc;
use swc_common::{FileName, SourceMap};
use swc_ecma_ast::{EsVersion, Module};
use swc_ecma_parser::{lexer::Lexer, Parser, StringInput, Syntax, TsConfig};

pub struct ParsedModule {
    pub module: Module,
    pub source_map: Lrc<SourceMap>,
}

pub fn parse_dts_file(path: &Path) -> Result<ParsedModule> {
    let cm: Lrc<SourceMap> = Default::default();
    let fm = cm
        .load_file(path)
        .context(format!("Failed to load file: {}", path.display()))?;

    let ts_config = TsConfig {
        tsx: false,
        decorators: true,
        dts: true,
        no_early_errors: true,
        ..Default::default()
    };

    let lexer = Lexer::new(
        Syntax::Typescript(ts_config),
        EsVersion::Es2022,
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);
    let module = parser
        .parse_module()
        .map_err(|e| {
            anyhow::anyhow!(
                "Failed to parse TypeScript declaration file: {:?}",
                e
            )
        })?;

    Ok(ParsedModule {
        module,
        source_map: cm,
    })
}

pub fn parse_dts_string(content: &str, filename: &str) -> Result<ParsedModule> {
    let cm: Lrc<SourceMap> = Default::default();
    let fm = cm.new_source_file(FileName::Custom(filename.to_string()), content.to_string());

    let ts_config = TsConfig {
        tsx: false,
        decorators: true,
        dts: true,
        no_early_errors: true,
        ..Default::default()
    };

    let lexer = Lexer::new(
        Syntax::Typescript(ts_config),
        EsVersion::Es2022,
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);
    let module = parser
        .parse_module()
        .map_err(|e| {
            anyhow::anyhow!(
                "Failed to parse TypeScript declaration file: {:?}",
                e
            )
        })?;

    Ok(ParsedModule {
        module,
        source_map: cm,
    })
}