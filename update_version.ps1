param (
    [string]$version = $null,
    [switch]$autoIncrement = $true
)

Function Get-AutoIncrementedVersion {
    Param ([string]$CurrentVersionString)

    $currentVersion = [System.Version]::Parse($CurrentVersionString.Split(" ")[1])
    $newVersionString = "{0}.{1}.{2}.{3}" -f ($currentVersion.Major, $currentVersion.Minor, $currentVersion.Build, $($currentVersion.Revision + 1))
    "Version: {0}" -f $newVersionString
}

$env:GIT_REDIRECT_STDERR = '2>&1'

$currentVersionString = @(cat "DESCRIPTION") -match "Version"

$newVersionString = &{if ($autoIncrement -eq $true) {Get-AutoIncrementedVersion -CurrentVersionString $currentVersionString} else {$version} }
((Get-Content -Path "DESCRIPTION" -Raw) -replace $currentVersionString, $newVersionString) | Set-Content -Path "DESCRIPTION" -NoNewline

$newVersion = ("v{0}" -f ($newVersionString | Select-String -Pattern "\d.+").Matches.Value)

git add .
git commit -m ("Update version: {0}" -f $newVersion)
git tag $newVersion
git push
git push --tags