use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ExpParser;

#[cfg(test)]
mod tests {
    use pest::parses_to;
    use pest::consumes_to;

    use super::*;
    #[test]
    fn integers() {
        parses_to! {
            parser: ExpParser,
            input: "123123123",
            rule: Rule::integer,
            tokens: [
                integer(0, 9)
            ]
        };
    }
}