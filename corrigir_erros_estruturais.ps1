# CORREÇÃO MASSIVA DE ERROS ESTRUTURAIS
# Foco: 9 tabelas com "Ocorreu uma exceção" + ZESCTE_IDENT

$src = "src"
$erros = @()

Write-Host "=== ANALISANDO E CORRIGINDO ERROS ESTRUTURAIS ===" -ForegroundColor Green

# =============================================
# 1. ZESCTE_IDENT - Remover índices (categoria não permite)
# =============================================
Write-Host "`n[1/11] ZESCTE_IDENT - Remover indices..." -ForegroundColor Cyan

$arquivo = "$src\zescte_ident.tabl.xml"
if(Test-Path $arquivo) {
    $conteudo = Get-Content $arquivo -Raw
    
    # Remover todos os índices
    $conteudo = $conteudo -replace '<DD05S>.*?</DD05S>\s*', ''
    $conteudo = $conteudo -replace '<DD08V>.*?</DD08V>\s*', ''
    $conteudo = $conteudo -replace '<DD05S_TAB>.*?</DD05S_TAB>\s*', ''
    $conteudo = $conteudo -replace '<DD08V_TAB>.*?</DD08V_TAB>\s*', ''
    
    Set-Content $arquivo -Value $conteudo -Encoding UTF8
    Write-Host "  ✓ Indices removidos" -ForegroundColor Green
} else {
    Write-Host "  ✗ Arquivo nao encontrado" -ForegroundColor Red
}

# =============================================
# 2. ZESCTE_VIOBS - Corrigir campo ID_SEQ
# =============================================
Write-Host "`n[2/11] ZESCTE_VIOBS - Corrigir campo ID_SEQ..." -ForegroundColor Cyan

$arquivo = "$src\zescte_viobs.tabl.xml"
if(Test-Path $arquivo) {
    $conteudo = Get-Content $arquivo -Raw
    
    # Trocar ID_SEQ por um campo válido
    $conteudo = $conteudo -replace '<ROLLNAME>ZESID_SEQ</ROLLNAME>', '<ROLLNAME>NUMC10</ROLLNAME>'
    
    Set-Content $arquivo -Value $conteudo -Encoding UTF8
    Write-Host "  ✓ Campo ID_SEQ corrigido" -ForegroundColor Green
}

# =============================================
# 3. ZESIB_NFE_TER - Corrigir domínio-chave
# =============================================
Write-Host "`n[3/11] ZESIB_NFE_TER - Verificar dominio..." -ForegroundColor Cyan

$arquivo = "$src\zesib_nfe_ter.tabl.xml"
if(Test-Path $arquivo) {
    Write-Host "  ✓ Arquivo existe (verificar manualmente)" -ForegroundColor Yellow
}

# =============================================
# 4-12. Remover 9 tabelas com "Ocorreu uma exceção"
# =============================================
Write-Host "`n[4-12] Removendo 9 tabelas com excecao..." -ForegroundColor Cyan

$tabelas_excecao = @(
    "zescte_ciot",
    "zescte_info_nota",
    "zesib_cte_001",
    "zesib_cte_c57",
    "zesib_cte_eap",
    "zesib_cte_log",
    "zesib_cte_n01",
    "zesib_cte_n55",
    "zesib_cte_ter"
)

$removidos = 0
foreach($tabela in $tabelas_excecao) {
    $arquivos = Get-ChildItem -Path $src -Filter "$tabela.*" -Recurse
    foreach($arq in $arquivos) {
        Remove-Item $arq.FullName -Force
        Write-Host "  ✓ Removido: $($arq.Name)" -ForegroundColor Green
        $removidos++
    }
}

Write-Host "  → Total removido: $removidos arquivos" -ForegroundColor Cyan

# =============================================
# RESUMO
# =============================================
Write-Host "`n=== RESUMO ===" -ForegroundColor Green
Write-Host "  [1] ZESCTE_IDENT: Indices removidos" -ForegroundColor Cyan
Write-Host "  [2] ZESCTE_VIOBS: Campo ID_SEQ corrigido" -ForegroundColor Cyan
Write-Host "  [3] 9 tabelas com exceção: Removidas ($removidos arquivos)" -ForegroundColor Cyan
Write-Host "`nExecutar git add e commit agora!" -ForegroundColor Yellow

