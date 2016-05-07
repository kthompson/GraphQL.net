module SchemaTypes

type Episode =
 | ANewHope
 | EmpireStrikesBack
 | ReturnOfTheJedi


type ICharacter =
    
    /// The id of the character.
    abstract member Id: string;
    
    /// The name of the character.
    abstract member Name: string;
    
    /// The friends of the character, or an empty list 
    /// if they have none.
    abstract member AppearsIn: list<Episode>;
    
    /// Which movies they appear in.
    abstract member Friends: list<ICharacter>;

type Human(id, name, appearsIn, friends, homePlanet : string) = 
    
    /// The home planet of the human, or null if unknown.
    member this.HomePlanet = homePlanet;

    interface ICharacter with
        
        /// The id of the character.
        member this.Id = id;
        
        /// The name of the character.
        member this.Name = name;
        
        /// The friends of the character, or an empty list 
        /// if they have none.
        member this.AppearsIn = appearsIn;
        
        /// Which movies they appear in.
        member this.Friends = friends;

type Droid(id, name, appearsIn, friends, primaryFunction) = 
    
    /// The primary function of the droid.
    member this.PrimaryFunction = primaryFunction;

    interface ICharacter with
    
        /// The id of the character.
        member this.Id = id;
    
        /// The name of the character.
        member this.Name = name;
    
        /// The friends of the character, or an empty list 
        /// if they have none.
        member this.AppearsIn = appearsIn;
    
        /// Which movies they appear in.
        member this.Friends = friends;



