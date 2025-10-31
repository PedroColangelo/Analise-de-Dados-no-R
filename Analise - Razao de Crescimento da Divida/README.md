# O Esforço Fiscal

 Este código em R trata de uma análise gráfica da sustentabilidade da dívida pública para 66 países do mundo, com destaque para o Brasil.

Utilizando as variáveis expectativa de taxa de juros nominais, expectativa de crescimento real, expectativa de inflação e expectativa de dívida/PIB, foi calculado o que chamei de "esforço fiscal" de cada país, que é simplesmente o valor do superávit primário necessário para estabilizar a dívida em relação ao PIB entre dois períodos t no tempo. O esforço fiscal do Brasil, tanto em termos absolutos, como em termos relativos aos demais países, terá que ser grande para compensar a expectativa de crescimento da dívida nos próximos 5 anos.

Além disso, na última seção do código foi realizada uma decomposição de Shapley com o propósito de tentar identificar quanto que cada uma das variáveis do estudo contribui para a diferença de esforço fiscal entre os países subdesenvolvidos que possuem esforço fiscal positivo.

Há 4 gráficos que acompanham a análise. O primeiro foca no esforço fiscal de cada país da amostra no período atual, dividindo os países entre desenvolvidos e subdesenvolvidos. O segundo possui a mesma lógica, mas dividindo os países entre asiáticos e não-asiáticos subdesenvolvidos com o intuito de observar com detalhes as diferenças de perfil fiscal entre os países asiáticos e o restante dos subdesenvolvidos. O terceiro é responsável por fazer uma comparação entre o esforço fiscal de 2024 e o esforço fiscal de 2025 para cada país. O quarto se trata do gráfico que exibe a decomposição de Shapley de cada um dos países desta subamostra.

Os dados foram extraídos do Fundo Monetário Internacional (projeção de crescimento real, inflação e dívida/PIB) e World Government Bonds (retorno de títulos públicos de 5 anos).