export interface ITruckDTO {
  id: string;
  truckID: string;
  tare: number;
  capacity: number;
  maxBatteryCapacity: number;
  autonomy: number;
  fastChargeTime: number;
  active: boolean;
}
