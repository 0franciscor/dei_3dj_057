import { Result } from "../../core/logic/Result";
import { ITruckDTO } from "../../dto/ITruckDTO";

export default interface ITruckService  {
    exist(truckID: string): Promise<Result<boolean>>;
    createTruck(truck: ITruckDTO): Promise<Result<ITruckDTO>>;
    getTruck(truckID: string): Promise<Result<ITruckDTO>>;
    getAllTrucks(): Promise<Result<ITruckDTO[]>>;
    updateTruck(truck: ITruckDTO): Promise<Result<ITruckDTO>>;
    softDeleteTruck(truckID: string): Promise<Result<ITruckDTO>>;
    hardDeleteTruck(truckID: string): Promise<Result<ITruckDTO>>;
}
