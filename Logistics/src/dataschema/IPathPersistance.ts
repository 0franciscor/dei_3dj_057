export interface IPathPersistance{
    domainId: string;
    pathID: string;
    startWHId: string;
    destinationWHId:string;
    pathDistance: number;
    pathTravelTime: number;
    wastedEnergy: number;
    extraTravelTime:number;
}