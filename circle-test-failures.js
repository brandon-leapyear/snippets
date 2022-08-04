let $uls = document.querySelectorAll("ul");
let $ul = $uls[$uls.length - 1];
let d = {};
Array.from($ul.querySelectorAll("li").values()).map($li => {
  let $testname = $li.querySelector('[data-testid=test-result-title]');
  let testname = $testname.textContent;
  let filename = $testname.parentElement.parentElement.children[1].textContent;

  // click on arrow if not expanded
  let $arrow = $li.querySelector('[aria-label="Arrow Drop Right"]');
  if ($arrow) {
    $arrow.parentElement.click();
  }

  let output = $li.querySelector("pre").textContent;
  let connectionError = output.match(/leapyear.exceptions.ConnectionError: .*$/m)?.[0];
  let hypothesisError = output.match(/hypothesis.errors.FailedHealthCheck: .*$/m)?.[0];

  d[filename] = d[filename] || [];
  d[filename].push({
    testname,
    error: connectionError || hypothesisError || output,
  });
})
console.log(JSON.stringify(d))