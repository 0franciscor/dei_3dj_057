using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using EletricGo.Domain.Shared;

namespace EletricGo.Domain.Products
{
    public class Product : Entity<ProductID>, IAggregateRoot
    {
        public string Description { get;  private set; }

        public CategoryID categoryID { get;  private set; }

        public bool Active{ get;  private set; }

        private Product()
        {
            this.Active = true;
        }

        public Product(string description, CategoryID catID)
        {
            if (categoryID == null)
                throw new BusinessRuleValidationException("Every product requires a category.");
            
            this.Id = new ProductId(Guid.NewGuid());
            this.Description = description;
            this.categoryID = catID;
            this.Active = true;
        }

        public void ChangeDescription(string description)
        {
            if (!this.Active)
                throw new BusinessRuleValidation("It is not possible to change the description to an inactive product.");
            this.Description = description;
        }

        public void ChangeCategoryId(CategoryId catId)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the category of an inactive product.");
            if (catId == null)
                throw new BusinessRuleValidationException("Every product requires a category.");
            this.categoryID = catId;;
        }
        public void MarkAsInative()
        {
            this.Active = false;
        }
    }
}