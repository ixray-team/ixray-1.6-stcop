using System.Collections;

namespace Flobbster.Windows.Forms {
    public class PropertyTable : PropertyBag {
        private Hashtable propValues;

        public object this[string key] {
            get {
                return propValues[key];
            }
            set {
                propValues[key] = value;
            }
        }

        public PropertyTable() {
            propValues = new Hashtable();
        }

        protected override void OnGetValue(PropertySpecEventArgs e) {
            e.Value = propValues[e.Property.Name];
            base.OnGetValue(e);
        }

        protected override void OnSetValue(PropertySpecEventArgs e) {
            propValues[e.Property.Name] = e.Value;
            base.OnSetValue(e);
        }
    }
}
