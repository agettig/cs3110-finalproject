type cards =
  | House of {
      name : string;
      price : int;
      selling_price : int;
      starter : bool;
    }
  | Career of {
      name : string;
      salary : int;
      salary_max : int;
      taxes_due : int;
      college_career : bool;
    }
  | Share_the_Wealth
  | Long_Term_Investment of int
  | Life_Tiles of int

(*These are all the career options in our game*)

let police_officer =
  Career
    {
      name = "Police Officer";
      salary = 40000;
      salary_max = 70000;
      taxes_due = 15000;
      college_career = false;
    }

let doctor =
  Career
    {
      name = "Doctor";
      salary = 100000;
      salary_max = 1000000;
      taxes_due = 45000;
      college_career = true;
    }

let lawyer =
  Career
    {
      name = "Lawyer";
      salary = 90000;
      salary_max = 1000000;
      taxes_due = 40000;
      college_career = true;
    }

let mechanic =
  Career
    {
      name = "Mechanic";
      salary = 30000;
      salary_max = 60000;
      taxes_due = 10000;
      college_career = false;
    }

let accountant =
  Career
    {
      name = "Accountant";
      salary = 70000;
      salary_max = 110000;
      taxes_due = 30000;
      college_career = true;
    }

let computer_designer =
  Career
    {
      name = "Computer Designer";
      salary = 50000;
      salary_max = 80000;
      taxes_due = 20000;
      college_career = true;
    }

let hair_stylist =
  Career
    {
      name = "Hair Stylist";
      salary = 30000;
      salary_max = 60000;
      taxes_due = 10000;
      college_career = false;
    }

let teacher =
  Career
    {
      name = "Teacher";
      salary = 40000;
      salary_max = 70000;
      taxes_due = 15000;
      college_career = true;
    }

let athlete =
  Career
    {
      name = "Athlete";
      salary = 60000;
      salary_max = 1000000;
      taxes_due = 25000;
      college_career = false;
    }

let veterinarian =
  Career
    {
      name = "Veterinarian";
      salary = 100000;
      salary_max = 120000;
      taxes_due = 35000;
      college_career = true;
    }

let entertainer =
  Career
    {
      name = "Entertainer";
      salary = 50000;
      salary_max = 1000000;
      taxes_due = 20000;
      college_career = false;
    }

let salesperson =
  Career
    {
      name = "Salesperson";
      salary = 20000;
      salary_max = 50000;
      taxes_due = 5000;
      college_career = false;
    }

let careers =
  [
    police_officer;
    salesperson;
    mechanic;
    hair_stylist;
    teacher;
    computer_designer;
    accountant;
    veterinarian;
    entertainer;
    lawyer;
    doctor;
    athlete;
  ]

let lg_tm_invt =
  [
    Long_Term_Investment 1;
    Long_Term_Investment 2;
    Long_Term_Investment 3;
    Long_Term_Investment 4;
    Long_Term_Investment 5;
    Long_Term_Investment 6;
    Long_Term_Investment 7;
    Long_Term_Investment 8;
    Long_Term_Investment 9;
  ]

let life_tiles =
  [
    Life_Tiles 10000;
    Life_Tiles 10000;
    Life_Tiles 10000;
    Life_Tiles 10000;
    Life_Tiles 10000;
    Life_Tiles 10000;
    Life_Tiles 10000;
    Life_Tiles 20000;
    Life_Tiles 20000;
    Life_Tiles 20000;
    Life_Tiles 20000;
    Life_Tiles 20000;
    Life_Tiles 20000;
    Life_Tiles 30000;
    Life_Tiles 30000;
    Life_Tiles 30000;
    Life_Tiles 30000;
    Life_Tiles 30000;
    Life_Tiles 40000;
    Life_Tiles 40000;
    Life_Tiles 40000;
    Life_Tiles 40000;
    Life_Tiles 50000;
    Life_Tiles 50000;
    Life_Tiles 50000;
  ]

let mobile_home =
  House
    {
      name = "Mobile Home";
      price = 80000;
      selling_price = 80000;
      starter = true;
    }

let condo =
  House
    {
      name = "Condo";
      price = 100000;
      selling_price = 105000;
      starter = true;
    }

let log_cabin =
  House
    {
      name = "Log Cabin";
      price = 120000;
      selling_price = 140000;
      starter = true;
    }

let ranch_style =
  House
    {
      name = "Ranch Style";
      price = 140000;
      selling_price = 160000;
      starter = true;
    }

let small_cape =
  House
    {
      name = "Small Cape";
      price = 160000;
      selling_price = 180000;
      starter = true;
    }

let tudor_style =
  House
    {
      name = "Tudor Style";
      price = 180000;
      selling_price = 200000;
      starter = true;
    }

let dbl_wide_rv =
  House
    {
      name = "Double Wide + RV";
      price = 300000;
      selling_price = 300000;
      starter = false;
    }

let executive_cape =
  House
    {
      name = "Executive Cape";
      price = 400000;
      selling_price = 400000;
      starter = false;
    }

let modern_victorian =
  House
    {
      name = "Modern Victoria";
      price = 500000;
      selling_price = 500000;
      starter = false;
    }

let mt_retreat =
  House
    {
      name = "Luxury Mountain Retreat";
      price = 600000;
      selling_price = 600000;
      starter = false;
    }

let penthouse =
  House
    {
      name = "Penthouse Suite";
      price = 700000;
      selling_price = 700000;
      starter = false;
    }

let mansion =
  House
    {
      name = "Mansion";
      price = 800000;
      selling_price = 800000;
      starter = false;
    }

let none =
  House { name = "None"; price = 0; selling_price = 0; starter = true }

let houses =
  [
    none;
    mobile_home;
    dbl_wide_rv;
    condo;
    executive_cape;
    log_cabin;
    modern_victorian;
    ranch_style;
    mt_retreat;
    small_cape;
    penthouse;
    tudor_style;
    mansion;
  ]
