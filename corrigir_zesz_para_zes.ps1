# Script para corrigir objetos ZESZ* para ZES* (remover Z duplicado)
# Corrige tanto nomes de arquivos quanto conteúdo interno dos XMLs

$ErrorActionPreference = "Stop"

Write-Host "================================================================" -ForegroundColor Cyan
Write-Host "CORREÇÃO MASSIVA: ZESZ* -> ZES*" -ForegroundColor Cyan
Write-Host "================================================================" -ForegroundColor Cyan
Write-Host ""

$srcPath = "src"
$logFile = "correcao_zesz_zes.log"

# Contadores
$totalArquivos = 0
$arquivosRenomeados = 0
$conteudosAtualizados = 0
$referenciasAtualizadas = 0
$erros = 0

# Criar log
"INICIO: $(Get-Date)" | Out-File $logFile

Write-Host "FASE 1: Identificando arquivos ZESZ*..." -ForegroundColor Yellow

# Obter todos os arquivos que começam com zesz
$arquivosZESZ = Get-ChildItem -Path $srcPath -Filter "zesz*.xml" -File
$totalArquivos = $arquivosZESZ.Count

Write-Host "Total de arquivos ZESZ* encontrados: $totalArquivos" -ForegroundColor White
"Total de arquivos ZESZ*: $totalArquivos" | Out-File $logFile -Append

if ($totalArquivos -eq 0) {
    Write-Host "Nenhum arquivo ZESZ* encontrado!" -ForegroundColor Red
    exit
}

Write-Host ""
Write-Host "FASE 2: Renomeando arquivos e atualizando conteúdo..." -ForegroundColor Yellow
Write-Host ""

$progressCount = 0

foreach ($arquivo in $arquivosZESZ) {
    $progressCount++
    
    # Mostrar progresso a cada 100 arquivos
    if ($progressCount % 100 -eq 0 -or $progressCount -eq 1) {
        $percentual = [math]::Round(($progressCount / $totalArquivos) * 100, 2)
        Write-Host "Progresso: $progressCount / $totalArquivos ($percentual%)" -ForegroundColor Cyan
    }
    
    try {
        $nomeAntigo = $arquivo.Name
        $caminhoAntigo = $arquivo.FullName
        
        # Novo nome: remover o segundo Z (zesz -> zes)
        $nomeNovo = $nomeAntigo -replace '^zesz', 'zes'
        $caminhoNovo = Join-Path $arquivo.Directory.FullName $nomeNovo
        
        # Ler conteúdo do arquivo
        $conteudo = Get-Content $caminhoAntigo -Raw -Encoding UTF8
        
        # Atualizar conteúdo: ZESZ -> ZES em vários contextos
        $conteudoAtualizado = $conteudo
        
        # Substituir em tags XML comuns
        $conteudoAtualizado = $conteudoAtualizado -replace '<ROLLNAME>ZESZ', '<ROLLNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<DOMNAME>ZESZ', '<DOMNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<TABNAME>ZESZ', '<TABNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<TYPENAME>ZESZ', '<TYPENAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<STRUCOBJN>ZESZ', '<STRUCOBJN>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<VIEWNAME>ZESZ', '<VIEWNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<SQLTAB>ZESZ', '<SQLTAB>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<ENTITYNAME>ZESZ', '<ENTITYNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<SHLPNAME>ZESZ', '<SHLPNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<ENQNAME>ZESZ', '<ENQNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<TCODE>ZESZ', '<TCODE>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<MSAG>ZESZ', '<MSAG>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<PROGNAME>ZESZ', '<PROGNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<CLSNAME>ZESZ', '<CLSNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<CMPNAME>ZESZ', '<CMPNAME>ZES'
        
        # Referências em domínios e elementos de dados
        $conteudoAtualizado = $conteudoAtualizado -replace '<DOMNAME>ZESZ', '<DOMNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<REFNAME>ZESZ', '<REFNAME>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<REFTABLE>ZESZ', '<REFTABLE>ZES'
        $conteudoAtualizado = $conteudoAtualizado -replace '<PRECFIELD>ZESZ', '<PRECFIELD>ZES'
        
        # Verificar se houve mudanças no conteúdo
        if ($conteudo -ne $conteudoAtualizado) {
            $conteudosAtualizados++
        }
        
        # Salvar com novo nome
        $conteudoAtualizado | Out-File -FilePath $caminhoNovo -Encoding utf8 -NoNewline
        
        # Remover arquivo antigo (só se o novo foi criado com sucesso)
        if (Test-Path $caminhoNovo) {
            Remove-Item $caminhoAntigo -Force
            $arquivosRenomeados++
            
            # Log detalhado a cada 500 arquivos
            if ($progressCount % 500 -eq 0) {
                "$progressCount/$totalArquivos - Renomeado: $nomeAntigo -> $nomeNovo" | Out-File $logFile -Append
            }
        }
        
    }
    catch {
        $erros++
        Write-Host "ERRO ao processar $($arquivo.Name): $_" -ForegroundColor Red
        "ERRO: $($arquivo.Name) - $_" | Out-File $logFile -Append
    }
}

Write-Host ""
Write-Host "FASE 3: Atualizando referências em outros arquivos..." -ForegroundColor Yellow
Write-Host ""

# Agora atualizar TODAS as referências ZESZ* em TODOS os arquivos XML
$todosArquivos = Get-ChildItem -Path $srcPath -Filter "*.xml" -File
$totalReferencias = $todosArquivos.Count
$progressCount = 0

foreach ($arquivo in $todosArquivos) {
    $progressCount++
    
    if ($progressCount % 200 -eq 0 -or $progressCount -eq 1) {
        $percentual = [math]::Round(($progressCount / $totalReferencias) * 100, 2)
        Write-Host "Atualizando referências: $progressCount / $totalReferencias ($percentual%)" -ForegroundColor Cyan
    }
    
    try {
        $conteudo = Get-Content $arquivo.FullName -Raw -Encoding UTF8
        $conteudoOriginal = $conteudo
        
        # Substituir todas as referências ZESZ por ZES
        $conteudo = $conteudo -replace '>ZESZ', '>ZES'
        $conteudo = $conteudo -replace '"ZESZ', '"ZES'
        $conteudo = $conteudo -replace "'ZESZ", "'ZES"
        $conteudo = $conteudo -replace ' ZESZ', ' ZES'
        $conteudo = $conteudo -replace '\.ZESZ', '.ZES'
        $conteudo = $conteudo -replace '/ZESZ', '/ZES'
        
        # Se houve alteração, salvar
        if ($conteudo -ne $conteudoOriginal) {
            $conteudo | Out-File -FilePath $arquivo.FullName -Encoding utf8 -NoNewline
            $referenciasAtualizadas++
        }
    }
    catch {
        Write-Host "ERRO ao atualizar referências em $($arquivo.Name): $_" -ForegroundColor Red
        "ERRO REFS: $($arquivo.Name) - $_" | Out-File $logFile -Append
    }
}

Write-Host ""
Write-Host "================================================================" -ForegroundColor Green
Write-Host "RESUMO DA CORREÇÃO ZESZ* -> ZES*" -ForegroundColor Green
Write-Host "================================================================" -ForegroundColor Green
Write-Host "Total de arquivos ZESZ* encontrados:    $totalArquivos" -ForegroundColor White
Write-Host "Arquivos renomeados:                     $arquivosRenomeados" -ForegroundColor Green
Write-Host "Conteúdos internos atualizados:          $conteudosAtualizados" -ForegroundColor Green
Write-Host "Arquivos com referências atualizadas:    $referenciasAtualizadas" -ForegroundColor Green
Write-Host "Erros encontrados:                       $erros" -ForegroundColor $(if($erros -gt 0){"Red"}else{"Green"})
Write-Host "================================================================" -ForegroundColor Green

# Salvar resumo no log
"
================================================================
RESUMO FINAL
================================================================
Total de arquivos ZESZ*: $totalArquivos
Arquivos renomeados: $arquivosRenomeados
Conteúdos atualizados: $conteudosAtualizados
Referências atualizadas: $referenciasAtualizadas
Erros: $erros
FIM: $(Get-Date)
================================================================
" | Out-File $logFile -Append

Write-Host ""
Write-Host "Log salvo em: $logFile" -ForegroundColor Cyan
Write-Host ""

if ($arquivosRenomeados -eq $totalArquivos -and $erros -eq 0) {
    Write-Host "✓ CORREÇÃO CONCLUÍDA COM SUCESSO!" -ForegroundColor Green
    Write-Host ""
    Write-Host "Próximos passos:" -ForegroundColor Yellow
    Write-Host "1. Verificar mudanças: git status" -ForegroundColor White
    Write-Host "2. Adicionar ao Git: git add src/" -ForegroundColor White
    Write-Host "3. Commit: git commit -m 'Corrigir nomes ZESZ* para ZES* - Remover Z duplicado'" -ForegroundColor White
    Write-Host "4. Push: git push origin main" -ForegroundColor White
}
else {
    Write-Host "⚠ CORREÇÃO CONCLUÍDA COM AVISOS!" -ForegroundColor Yellow
    Write-Host "Verifique o log para detalhes: $logFile" -ForegroundColor White
}

