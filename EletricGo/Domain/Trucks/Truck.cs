using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Trucks 
{
    public class Truck : Entity<TruckID>
    {

        private TruckID truckID { get; set; }
        private Tare tare { get; set; }
        private Capacity capacity { get; set; }
        private MaxBatteryCapacity maxBatteryCapacity { get; set; }
        private Autonomy autonomy { get; set; }
        private FastChargeTime fastChargeTime { get; set; }

        public Truck(TruckID truckID, Tare tare, Capacity capacity, MaxBatteryCapacity maxBatteryCapacity, Autonomy autonomy, FastChargeTime fastChargeTime)
        {
            this.Id = new TruckID(Guid.NewGuid());
            this.truckID = truckID;
            this.tare = tare;
            this.capacity = capacity;
            this.maxBatteryCapacity = maxBatteryCapacity;
            this.autonomy = autonomy;
            this.fastChargeTime = fastChargeTime;
        }


    }
}


