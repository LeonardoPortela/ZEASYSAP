# Corrigir ZESES* para ZES* - Remover ES duplicado
$ErrorActionPreference = "Stop"

Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "CORRECAO: ZESES* -> ZES* (Remover ES duplicado)" -ForegroundColor Cyan
Write-Host "================================================================" -ForegroundColor Cyan
Write-Host ""

$srcPath = "src"
$arquivosRenomeados = 0
$conteudosAtualizados = 0
$referenciasAtualizadas = 0

# Fase 1: Renomear arquivos ZESES*
Write-Host "Fase 1: Renomeando arquivos ZESES*..." -ForegroundColor Yellow
$arquivosZESES = Get-ChildItem -Path $srcPath -Filter "zeses*.xml" -File
$totalArquivos = $arquivosZESES.Count

Write-Host "Total de arquivos ZESES*: $totalArquivos" -ForegroundColor White
Write-Host ""

foreach ($arquivo in $arquivosZESES) {
    $nomeAntigo = $arquivo.Name
    $caminhoAntigo = $arquivo.FullName
    
    # zeses -> zes (remover um ES)
    $nomeNovo = $nomeAntigo -replace '^zeses', 'zes'
    $caminhoNovo = Join-Path $arquivo.Directory.FullName $nomeNovo
    
    Write-Host "  $nomeAntigo -> $nomeNovo" -ForegroundColor Gray
    
    # Ler conteudo
    $conteudo = Get-Content $caminhoAntigo -Raw -Encoding UTF8
    
    # Atualizar conteudo interno: ZESES -> ZES
    $conteudoAtualizado = $conteudo -replace '<ROLLNAME>ZESES', '<ROLLNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<DOMNAME>ZESES', '<DOMNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<TABNAME>ZESES', '<TABNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<TYPENAME>ZESES', '<TYPENAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<STRUCOBJN>ZESES', '<STRUCOBJN>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<VIEWNAME>ZESES', '<VIEWNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<SQLTAB>ZESES', '<SQLTAB>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<ENTITYNAME>ZESES', '<ENTITYNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<REFNAME>ZESES', '<REFNAME>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<REFTABLE>ZESES', '<REFTABLE>ZES'
    $conteudoAtualizado = $conteudoAtualizado -replace '<PRECFIELD>ZESES', '<PRECFIELD>ZES'
    
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
Write-Host "Fase 2: Atualizando referencias ZESES* -> ZES*..." -ForegroundColor Yellow
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
    
    # Substituir referencias ZESES por ZES
    $conteudo = $conteudo -replace '>ZESES', '>ZES'
    $conteudo = $conteudo -replace '"ZESES', '"ZES'
    $conteudo = $conteudo -replace ' ZESES', ' ZES'
    $conteudo = $conteudo -replace '\.ZESES', '.ZES'
    $conteudo = $conteudo -replace '/ZESES', '/ZES'
    
    if ($conteudo -ne $conteudoOriginal) {
        $conteudo | Out-File -FilePath $arquivo.FullName -Encoding utf8 -NoNewline
        $referenciasAtualizadas++
    }
}

Write-Host ""
Write-Host "================================================================" -ForegroundColor Green
Write-Host "RESUMO DA CORRECAO" -ForegroundColor Green
Write-Host "================================================================" -ForegroundColor Green
Write-Host "Total de arquivos ZESES*: $totalArquivos" -ForegroundColor White
Write-Host "Arquivos renomeados: $arquivosRenomeados" -ForegroundColor Green
Write-Host "Conteudos atualizados: $conteudosAtualizados" -ForegroundColor Green
Write-Host "Referencias atualizadas: $referenciasAtualizadas" -ForegroundColor Green
Write-Host "================================================================" -ForegroundColor Green
Write-Host "Concluido!" -ForegroundColor Green

