param([string]$rScript = "$PSScriptRoot\runShinyApp.R")

if ($rScript -and (Test-Path $rScript)) {
	[string[]]$params = @($rScript, $PSScriptRoot, '--no-save', '--no-environ', '--no-init', '--no-restore') #, '--no-Rconsole')

	[string]$log = "$PSScriptRoot\logs\err.log"

	Start-Process -FilePath "$PSScriptRoot\R-Portable\App\R-Portable\bin\Rscript.exe" -ArgumentList $params -Wait -RedirectStandardError $log # -WindowStyle Hidden
}
