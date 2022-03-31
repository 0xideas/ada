package ada.interface

import ada._

trait UpdateableContext[Context]{
    def update(context: Context, reward: ada.Reward): Unit
}

trait Updateable{
    def update(reward: ada.Reward): Unit
}
