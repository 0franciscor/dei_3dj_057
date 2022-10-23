import { Service, Inject } from 'typedi';
import { Result } from "../core/logic/Result";
import config from "../../config";
import { ITruckDTO } from "../dto/ITruckDTO";
import ITruckRepo from './IRepos/ITruckRepo';
import ITruckService from './IServices/ITruckService';
import { Truck } from "../domain/truck/Truck";


import { TruckMap } from '../mappers/TruckMap';
import { Autonomy } from '../domain/truck/Autonomy';
import { Tare } from '../domain/truck/Tare';
import { Capacity } from '../domain/truck/Capacity';
import { MaxBatteryCapacity } from '../domain/truck/MaxBatteryCapacity';
import { FastChargeTime } from '../domain/truck/FastChargeTime';


@Service()
export class TruckService implements ITruckService {
    constructor(
        @Inject(config.repos.truck.name) private truckRepo: ITruckRepo,
    ) { }

    
    public async createTruck(truckDTO: ITruckDTO): Promise<Result<ITruckDTO>> {
        try {
            
            const truckOrError = Truck.create(truckDTO);
            if (truckOrError.isFailure) {
                return Result.fail<ITruckDTO>(truckOrError.error);
            }
            const truckResult = truckOrError.getValue();
            await this.truckRepo.save(truckResult);

            const truckDTOResult = TruckMap.toDTO(truckResult) as ITruckDTO;
            return Result.ok<ITruckDTO>(truckDTOResult);


        } catch (e) {
            
            throw e;

        }
    }

    public async getTruck(truckID: string): Promise<Result<ITruckDTO>> {
        try { 
            const truck = await this.truckRepo.getTruckById(truckID);
            if(truck === null)
                return Result.fail<ITruckDTO>("Truck not found");

            const truckDTOResult = TruckMap.toDTO(truck) as ITruckDTO;
            return Result.ok<ITruckDTO>(truckDTOResult);
        } catch (e) {
            throw e;
        }
    }

    public async updateTruck(truckDTO: ITruckDTO): Promise<Result<ITruckDTO>> {
        try {
            const truck = await this.truckRepo.getTruckById(truckDTO.id);
            if(truck === null)
                return Result.fail<ITruckDTO>("Truck not found");

            truck.autonomy = Autonomy.create(truckDTO.autonomy).getValue();
            truck.tare = Tare.create(truckDTO.tare).getValue();
            truck.capacity = Capacity.create(truckDTO.capacity).getValue();
            truck.maxBatteryCapacity = MaxBatteryCapacity.create(truckDTO.maxBatteryCapacity).getValue();
            truck.fastChargeTime = FastChargeTime.create(truckDTO.fastChargeTime).getValue();

            await this.truckRepo.save(truck);

            const truckDTOResult = TruckMap.toDTO(truck) as ITruckDTO;
            return Result.ok<ITruckDTO>(truckDTOResult);
        } catch (e) {
            throw e;
        }

    }

    public async deleteTruck(truckID: string): Promise<Result<ITruckDTO>> {
        try {
            const truck = await this.truckRepo.getTruckById(truckID);
            if(truck === null)
                return Result.fail<ITruckDTO>("Truck not found");

            await this.truckRepo.delete(truck);

            const truckDTOResult = TruckMap.toDTO(truck) as ITruckDTO;
            return Result.ok<ITruckDTO>(truckDTOResult);
        } catch (e) {
            throw e;
        }
    }


}