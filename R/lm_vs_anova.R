data(hand_washing, package = 'VisualStats')
hand_washing$Method <- as.factor(hand_washing$Method)

VisualStats::describe_by(hand_washing$Bacterial_Counts, hand_washing$Method)
aov(Bacterial_Counts ~ Method, data = hand_washing) |> summary()
lm(Bacterial_Counts ~ Method, data = hand_washing) |> summary()

hand_washing$Method <- relevel(hand_washing$Method, ref = 'Water')
lm(Bacterial_Counts ~ Method, data = hand_washing) |> summary()

