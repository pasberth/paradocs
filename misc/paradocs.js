hljs.registerLanguage("paradocs", function(hljs) {
  return {
    c: [
      {
        cN: "keyword",
        v: [
          { b: "%rule" },
          { b: "%extend" },
          { b: "%def" },
          { b: "%escape" },
          { b: "%render-before" },
          { b: "%render-after" },
          { b: "%indent" },
          { b: "%include" },
          { b: "%read" },
        ]
      },
      {
        cN: 'string',
        v: [
          { b: '""""', e: '""""' },
          { b: '"""', e: '"""' },
          { b: '""', e: '""' },
          { b: '"', e: '"' },
        ]
      },
      {
        cN: 'symbol',
        b: /%\S+/
      }
    ]
  };
})