import { Repo } from '../../core/infra/Repo';
import { Truck } from '../../domain/truck/Truck';

export default interface ITruckRepo extends Repo<Truck> {
    exists(truck: Truck): Promise<boolean>;
    save(truck: Truck): Promise<Truck>;
    delete(truck: Truck): Promise<Truck>;
    getTruckById(id: string): Promise<Truck>;
    
}   