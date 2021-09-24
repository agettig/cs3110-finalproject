type cards = 
|House of {name: string; price:int; selling_price:int}
|Career of {name: string; salary: int; salary_max: int; taxes_due: int; college: bool}
|Share_the_Wealth
|Long_Term_Investment of int
|Life_Tiles of int

type player = {name: string; children: int; so: bool; deck: cards list; 
<<<<<<< HEAD
account_balance: int; debt:int; pay_raise:int }

type tiles =
| PayTile
| DebtTile
| LifeTile
| CareerTile
| FamilyTile
| HouseTile


=======
account_balance: int; debt:int; pay_raise:int; college: bool}
>>>>>>> ff0e0411805f6ea68b1246f19d712eb99e52f08c
