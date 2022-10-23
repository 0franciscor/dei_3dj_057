export interface ITruckPersistence {
    domainId: string;
    truckID: string;
    tare: number;
    capacity: number;
    maxBatteryCapacity: number;
    autonomy: number;
    fastChargeTime: number;
}