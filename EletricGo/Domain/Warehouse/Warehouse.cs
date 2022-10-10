using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouse;

namespace EletricGo.Domain.Products
{
    public class Warehouse : Entity<WarehouseID> //: IAgg
    {
        public string Description { get;  private set; }

        public WarehouseID warehouseID { get;  private set; }

        public bool Active{ get;  private set; }

        private Warehouse()
        {
            this.Active = true;
        }

        public Warehouse(string description, WarehouseID whID)
        {
            if (warehouseID == null)
                //throw new BusinessRuleValidationException("Every product requires a category.");
            
            this.Id = new WarehouseID(Guid.NewGuid());
            this.Description = description;
            this.warehouseID = whID;
            this.Active = true;
        }

        public void ChangeDescription(string description)
        {
            if (!this.Active)
                //throw new BusinessRuleValidation("It is not possible to change the description to an inactive product.");
            this.Description = description;
        }

        public void ChangeWarehouseID(WarehouseID whID)
        {
            if (!this.Active)
                //throw new BusinessRuleValidationException("It is not possible to change the category of an inactive product.");
            if (whID == null)
                //throw new BusinessRuleValidationException("Every product requires a category.");
            this.warehouseID = warehouseID;
        }
        public void MarkAsInative()
        {
            this.Active = false;
        }
    }
}