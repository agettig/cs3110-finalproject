type cards = 
|House of {name: string; price:int; selling_price:int}
|Career of {name: string; salary: int; salary_max: int; taxes_due: int; college: bool}
|Share_the_Wealth
|Long_Term_Investment of int
|Life_Tiles of int

type player = {name: string; children: int; so: bool; deck: cards list; 
account_balance: int; debt:int; pay_raise:int; college: bool}