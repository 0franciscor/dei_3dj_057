import { Service, Inject } from 'typedi';
import { Result } from "../core/logic/Result";
import config from "../../config";
import { ITruckDTO } from "../dto/ITruckDTO";
import ITruckRepo from '../repos/IRepos/ITruckRepo';
import ITruckService from './IServices/ITruckService';
import { Truck } from "../domain/truck/Truck";


import { TruckMap } from '../mappers/TruckMap';
import { Autonomy } from '../domain/truck/Autonomy';
import { Tare } from '../domain/truck/Tare';
import { Capacity } from '../domain/truck/Capacity';
import { MaxBatteryCapacity } from '../domain/truck/MaxBatteryCapacity';
import { FastChargeTime } from '../domain/truck/FastChargeTime';



@Service()
export default class TruckService implements ITruckService {
    constructor(
        @Inject(config.repos.truck.name) private truckRepo: ITruckRepo,
    ) { }


    public async exist(truckID: string): Promise<Result<boolean>> {
        try {
            const truckResult = await this.truckRepo.getTruckById(truckID);
            if(truckResult === null)
                return Result.ok<boolean>(false);
            return Result.ok<boolean>(true);
        } catch (e) {
            throw e;
        }
    }



    public async createTruck(truckDTO: ITruckDTO): Promise<Result<ITruckDTO>> {
        try {

            const truck = await this.truckRepo.getTruckById(truckDTO.truckID);
            if(truck !== null)
                return Result.fail<ITruckDTO>("Truck already exists");
            const truckOrError = Truck.create(truckDTO);

            if (truckOrError.isFailure) {
                return Result.fail<ITruckDTO>(truckOrError.error);
            }
            const truckResult = truckOrError.getValue();
            console.log(truckResult);
            await this.truckRepo.save(truckResult);
            
            const truckDTOResult = TruckMap.toDTO(truckResult) as ITruckDTO;
            console.log(truckDTOResult);
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
            console.log(truckDTOResult)
            return Result.ok<ITruckDTO>(truckDTOResult);
        } catch (e) {
            throw e;
        }
    }

    public async getAllTrucks(): Promise<Result<ITruckDTO[]>> {

        try {
            const trucks = await this.truckRepo.getAllTrucks();
            const truckDTOResult = TruckMap.toDTOList(trucks) as ITruckDTO[];
            return Result.ok<ITruckDTO[]>(truckDTOResult);
        } catch (e) {
            throw e;
        }

    }

    public async updateTruck(truckDTO: ITruckDTO): Promise<Result<ITruckDTO>> {
        try {
            const truck = await this.truckRepo.getTruckById(truckDTO.truckID);
            if(truck === null)
                return Result.fail<ITruckDTO>("Truck not found");
            if(truckDTO.autonomy != truck.autonomy.autonomy && truckDTO.autonomy != null)
                truck.autonomy = Autonomy.create(truckDTO.autonomy).getValue();
            if(truckDTO.tare != truck.tare.tare && truckDTO.tare != null)
                truck.tare = Tare.create(truckDTO.tare).getValue();
            if(truckDTO.capacity != truck.capacity.capacity && truckDTO.capacity != null)
                truck.capacity = Capacity.create(truckDTO.capacity).getValue();
            if(truckDTO.maxBatteryCapacity != truck.maxBatteryCapacity.capacity && truckDTO.maxBatteryCapacity != null)
                truck.maxBatteryCapacity = MaxBatteryCapacity.create(truckDTO.maxBatteryCapacity).getValue();
            if(truckDTO.fastChargeTime != truck.fastChargeTime.time && truckDTO.fastChargeTime != null)
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