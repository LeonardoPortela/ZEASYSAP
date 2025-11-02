# Corrigir ZESZ* restantes para ZES* - Remover Z duplicado
$ErrorActionPreference = "Stop"

Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "CORRECAO: ZESZ* -> ZES* (Remover Z duplicado - RESTANTES)" -ForegroundColor Cyan
Write-Host "================================================================" -ForegroundColor Cyan
Write-Host ""

$srcPath = "src"
$arquivosRenomeados = 0
$conteudosAtualizados = 0
$referenciasAtualizadas = 0

# Fase 1: Renomear arquivos ZESZ*
Write-Host "Fase 1: Renomeando arquivos ZESZ*..." -ForegroundColor Yellow
$arquivosZESZ = Get-ChildItem -Path $srcPath -Filter "zesz*.xml" -File
$totalArquivos = $arquivosZESZ.Count

Write-Host "Total de arquivos ZESZ*: $totalArquivos" -ForegroundColor White
Write-Host ""

foreach ($arquivo in $arquivosZESZ) {
    $nomeAntigo = $arquivo.Name
    $caminhoAntigo = $arquivo.FullName
    
    # zesz -> zes (remover um Z)
    $nomeNovo = $nomeAntigo -replace '^zesz', 'zes'
    $caminhoNovo = Join-Path $arquivo.Directory.FullName $nomeNovo
    
    Write-Host "  $nomeAntigo -> $nomeNovo" -ForegroundColor Gray
    
    # Ler conteudo
    $conteudo = Get-Content $caminhoAntigo -Raw -Encoding UTF8
    
    # Atualizar conteudo interno: ZESZ -> ZES
    $conteudoAtualizado = $conteudo -replace '<ROLLNAME>ZESZ', '<ROLLNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<DOMNAME>ZESZ', '<DOMNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<TABNAME>ZESZ', '<TABNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<TYPENAME>ZESZ', '<TYPENAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<STRUCOBJN>ZESZ', '<STRUCOBJN>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<VIEWNAME>ZESZ', '<VIEWNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<SQLTAB>ZESZ', '<SQLTAB>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<ENTITYNAME>ZESZ', '<ENTITYNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<REFNAME>ZESZ', '<REFNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<REFTABLE>ZESZ', '<REFTABLE>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<PRECFIELD>ZESZ', '<PRECFIELD>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<PROGNAME>ZESZ', '<PROGNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<CLSNAME>ZESZ', '<CLSNAME>ZES'
    
    if ($conteudo -ne $conteudoAtualizado) {
        $conteudosAtualizados++
    }
    
    # Salvar com novo nome
    $conteudoAtualizado | Out-File -FilePath $caminhoNovo -Encoding utf8 -NoNewline
    
    # Remover arquivo antigo
    if (Test-Path $caminhoNovo) {
        Remove-Item $caminhoAntigo -Force
        $arquivosRenomeados++
    }
}

# Fase 2: Atualizar referencias em todos os arquivos
Write-Host ""
Write-Host "Fase 2: Atualizando referencias ZESZ* -> ZES*..." -ForegroundColor Yellow
Write-Host ""

$todosArquivos = Get-ChildItem -Path $srcPath -Filter "*.xml" -File
$totalReferencias = $todosArquivos.Count
$progressCount = 0

foreach ($arquivo in $todosArquivos) {
    $progressCount++
    
    if ($progressCount % 100 -eq 0) {
        $percentual = [math]::Round(($progressCount / $totalReferencias) * 100, 2)
        Write-Host "Progresso: $progressCount / $totalReferencias ($percentual%)" -ForegroundColor Cyan
    }
    
    $conteudo = Get-Content $arquivo.FullName -Raw -Encoding UTF8
    $conteudoOriginal = $conteudo
    
    # Substituir referencias ZESZ por ZES
    $conteudo = $conteudo -replace '>ZESZ', '>ZES'
    $conteudo = $conteudo -replace '"ZESZ', '"ZES'
    $conteudo = $conteudo -replace ' ZESZ', ' ZES'
    $conteudo = $conteudo -replace '\.ZESZ', '.ZES'
    $conteudo = $conteudo -replace '/ZESZ', '/ZES'
    
    if ($conteudo -ne $conteudoOriginal) {
        $conteudo | Out-File -FilePath $arquivo.FullName -Encoding utf8 -NoNewline
        $referenciasAtualizadas++
    }
}

Write-Host ""
Write-Host "================================================================" -ForegroundColor Green
Write-Host "RESUMO DA CORRECAO" -ForegroundColor Green
Write-Host "================================================================" -ForegroundColor Green
Write-Host "Total de arquivos ZESZ*: $totalArquivos" -ForegroundColor White
Write-Host "Arquivos renomeados: $arquivosRenomeados" -ForegroundColor Green
Write-Host "Conteudos atualizados: $conteudosAtualizados" -ForegroundColor Green
Write-Host "Referencias atualizadas: $referenciasAtualizadas" -ForegroundColor Green
Write-Host "================================================================" -ForegroundColor Green
Write-Host "Concluido!" -ForegroundColor Green

