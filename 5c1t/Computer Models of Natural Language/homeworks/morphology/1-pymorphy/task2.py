import pymorphy2


morph = pymorphy2.MorphAnalyzer()

print('1. лемму слова “руки”.')
p = morph.parse('руки')
print(p[0].normal_form)
print()

print('2. возможные грамматические значения слова “руки” и их оценки.')
result = '\n'.join(map(lambda gr: f'{gr.tag} -- {gr.score}', p))
print(result)
print()

print('3. леммы слова “три” и их оценки с учётом всех их грамматических значений.')
p = morph.parse('три')
result = '\n'.join(map(lambda gr: f'{gr.normal_form} -- {gr.score}', p))
print(result)
print()

print('4. лексему леммы “стать” - в виде набора слов и их оценок.')
ps = morph.parse('стать')
for p in ps:
    result = '\n'.join(map(lambda gr: f'{gr.word} -- {gr.score}', p.lexeme))
    print(result)
    print('-' * 10)
print()

print('5. множественное число, творительный падеж от леммы “турок”.')
p = morph.parse('турок')[0]
result = p.inflect({'plur', 'ablt'})
print(result.word)
print()

print('6. часть речи слова “майню”.')
p = morph.parse('майню')[0]
result = morph.lat2cyr(p.tag.POS)
print(result)
