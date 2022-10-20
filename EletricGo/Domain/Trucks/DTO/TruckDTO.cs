namespace EletricGo.Domain.Trucks.DTO
{
    public class TruckDTO
    {

        public Guid truckID { get; set; }
        public float tare { get; set; }
        public float capacity { get; set; }
        public float maxBatteryCapacity { get; set; }
        public float autonomy { get; set; }
        public float fastChargeTime { get; set; }

        public TruckDTO(Guid truckID, float tare, float capacity, float maxBatteryCapacity, float autonomy, float fastChargeTime)
        {
            this.truckID = truckID;
            this.tare = tare;
            this.capacity = capacity;
            this.maxBatteryCapacity = maxBatteryCapacity;
            this.autonomy = autonomy;
            this.fastChargeTime = fastChargeTime;
        }

    }
    
}