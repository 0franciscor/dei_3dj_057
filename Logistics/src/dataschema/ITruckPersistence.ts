export interface ITruckPersistence {
    domainId: string;
    tare: number;
    capacity: number;
    maxBatteryCapacity: number;
    autonomy: number;
    fastChargeTime: number;
}