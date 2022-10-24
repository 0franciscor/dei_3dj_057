export interface IPathDTO{
    id: string; 
    pathID: string;
    startWHId: string;
    destinationWHId: string;
    pathDistance: number;
    pathTravelTime: number;  
    wastedEnergy: number;
    extraTravelTime: number;
}