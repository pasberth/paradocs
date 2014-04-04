hljs.registerLanguage("paradocs", function(hljs) {
  return {
    c: [
      {
        cN: 'string',
        v: [
          { b: '%\\\\.?' },
          { b: '%""""', e: '""""' },
          { b: '%"""', e: '"""' },
          { b: '%""', e: '""' },
          { b: '%"', e: '"' },
          { b: '\\\\.?' },
          { b: '""""', e: '""""' },
          { b: '"""', e: '"""' },
          { b: '""', e: '""' },
          { b: '"', e: '"' },
        ]
      },
      {
        cN: "keyword",
        v: [
          { b: "%rule" },
          { b: "%extend" },
          { b: "%def" },
          { b: "%escape" },
          { b: "%before" },
          { b: "%after" },
          { b: "%include" },
          { b: "%read" },
        ]
      },
      {
        cN: 'symbol',
        b: /%\S+/
      }
    ]
  };
})