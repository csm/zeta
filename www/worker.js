let repl = null;

function post(type, payload = {}) {
  self.postMessage({ type, ...payload });
}

async function ev(code) {
  const r = await repl.eval(code);
  if (r.is_error) throw new Error(r.result);
  return { output: r.output, result: r.result };
}

function splitFirstForm(src) {
  let depth = 0, inStr = false, esc = false, inComment = false, start = -1;
  for (let i = 0; i < src.length; i++) {
    const ch = src[i];
    if (inComment) { if (ch === '\n') inComment = false; continue; }
    if (inStr) {
      if (esc) esc = false;
      else if (ch === '\\') esc = true;
      else if (ch === '"') inStr = false;
      continue;
    }
    if (ch === ';') { inComment = true; continue; }
    if (ch === '"') { inStr = true; continue; }
    if (ch === '(') { if (depth === 0) start = i; depth++; }
    else if (ch === ')') {
      depth--;
      if (depth === 0) return [src.slice(start, i + 1), src.slice(i + 1)];
    }
  }
  throw new Error('unbalanced parens in source');
}

function adaptForRepl(src) {
  const [nsForm, body] = splitFirstForm(src);
  const nsName = nsForm.match(/\(ns\s+([\w.\-]+)/)[1];
  let out = body;
  for (const [, full, alias] of nsForm.matchAll(/\[([\w.\-]+)\s+:as\s+([\w\-]+)\]/g)) {
    out = out.replace(new RegExp(`(^|[\\s()\\[{])${alias}/`, 'g'), `$1${full}/`);
  }
  return `(ns ${nsName})` + out;
}

async function fetchSource(name) {
  for (const prefix of ['../src/zeta/', './src/zeta/', '/src/zeta/']) {
    try {
      const resp = await fetch(prefix + name);
      if (resp.ok) return await resp.text();
    } catch (_) { /* try next */ }
  }
  throw new Error('could not fetch src/zeta/' + name +
    ' — serve the repository root, not just www/');
}

async function boot() {
  post('status', { message: 'loading wasm…' });
  const mod = await import('./pkg/cljrs_wasm.js');
  await mod.default();
  repl = new mod.Repl();
  const files = ['complex.cljc', 'gamma.cljc', 'core.cljc', 'viz.cljc'];
  for (let i = 0; i < files.length; i++) {
    const f = files[i];
    post('progress', { message: 'loading ' + f + '…', value: i, max: files.length });
    const src = await fetchSource(f);
    await ev(adaptForRepl(src));
  }
  post('ready');
}

self.onmessage = async event => {
  const { id, type, code } = event.data;
  if (type !== 'eval') return;
  try {
    if (!repl) throw new Error('worker is not ready yet');
    const result = await ev(code);
    post('result', { id, ...result });
  } catch (e) {
    post('error', { id, message: String(e.message || e) });
  }
};

boot().catch(e => post('boot-error', { message: String(e.message || e) }));
