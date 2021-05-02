bot <- telegram.bot::Bot(token = bot_token)
print(bot$getMe())
chat_id <- "505032933"



# Create Custom Keyboard
text <- "Aren't those custom keyboards cool?"
RKM <- ReplyKeyboardMarkup(
  keyboard = list(
    list(KeyboardButton("Yes, they certainly are!")),
    list(KeyboardButton("I'm not quite sure")),
    list(KeyboardButton("No..."))
  ),
  resize_keyboard = FALSE,
  one_time_keyboard = TRUE
)
# Send Custom Keyboard
bot$sendMessage(chat_id, text, reply_markup = RKM)
text <- "Don't forget to send me the answer!"
RKM <- ReplyKeyboardMarkup(
  keyboard = list(
    list(KeyboardButton("Yes, they certainly are!")),
    list(KeyboardButton("I'm not quite sure")),
    list(KeyboardButton("No..."))
  ),
  resize_keyboard = FALSE,
  one_time_keyboard = FALSE
)
# Send Custom Keyboard
bot$sendMessage(chat_id, text, reply_markup = RKM)
# Remove Keyboard
bot$sendMessage(
  chat_id,
  "Okay, thanks!",
  reply_markup = ReplyKeyboardRemove()
)








text <- "Don't forget to send me the answer!"
# Send reply message
bot$sendMessage(chat_id, text, reply_markup = ForceReply())









