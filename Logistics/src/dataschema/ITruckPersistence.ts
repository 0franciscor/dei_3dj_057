export interface ITruckPersistence {
    id: string;
    tare: number;
    capacity: number;
    maxBatteryCapacity: number;
    autonomy: number;
    fastChargeTime: number;
}