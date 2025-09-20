pos <- saxo_get_positions()
str(pos, max.level = 1)
# esim. taulukoksi:
positions <- pos$Data
head(positions[c("AccountId","NetPositionId","Uic","AssetType","Amount","OpenPrice")])
