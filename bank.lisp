(defgeneric withdraw (account amount)
  (:documentation "acountで指定された学を口座から引き落とす
現在の残高がamountより少なかったらエラーを通知する"))
