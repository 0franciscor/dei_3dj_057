import { Result } from "../../core/logic/Result";
import { ITruckDTO } from "../../dto/ITruckDTO";

export default interface ITruckService  {
    createTruck(truck: ITruckDTO): Promise<Result<ITruckDTO>>;
    getTruck(truckID: string): Promise<Result<ITruckDTO>>;
    updateTruck(truck: ITruckDTO): Promise<Result<ITruckDTO>>;
    deleteTruck(truckID: string): Promise<Result<ITruckDTO>>;
}
