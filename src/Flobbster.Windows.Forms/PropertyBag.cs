using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing.Design;

namespace Flobbster.Windows.Forms {
    public class PropertyBag : ICustomTypeDescriptor {
        [Serializable]
        public class PropertySpecCollection : IList, ICollection, IEnumerable {
            private ArrayList innerArray;

            object IList.this[int index] {
                get {
                    return this[index];
                }
                set {
                    this[index] = (PropertySpec) value;
                }
            }

            public PropertySpec this[int index] {
                get {
                    return (PropertySpec) innerArray[index];
                }
                set {
                    innerArray[index] = value;
                }
            }

            public int Count {
                get {
                    return innerArray.Count;
                }
            }

            public bool IsFixedSize {
                get {
                    return false;
                }
            }

            public bool IsReadOnly {
                get {
                    return false;
                }
            }

            public bool IsSynchronized {
                get {
                    return false;
                }
            }

            object ICollection.SyncRoot {
                get {
                    return null;
                }
            }

            int IList.Add(object value) {
                return Add((PropertySpec) value);
            }

            bool IList.Contains(object obj) {
                return Contains((PropertySpec) obj);
            }

            int IList.IndexOf(object obj) {
                return IndexOf((PropertySpec) obj);
            }

            void IList.Insert(int index, object value) {
                Insert(index, (PropertySpec) value);
            }

            void IList.Remove(object value) {
                Remove((PropertySpec) value);
            }

            public PropertySpecCollection() {
                innerArray = new ArrayList();
            }

            public int Add(PropertySpec value) {
                return innerArray.Add(value);
            }

            public void AddRange(PropertySpec[] array) {
                innerArray.AddRange(array);
            }

            public void Clear() {
                innerArray.Clear();
            }

            public bool Contains(PropertySpec item) {
                return innerArray.Contains(item);
            }

            public bool Contains(string name) {
                foreach (PropertySpec item in innerArray) {
                    if (item.Name == name) {
                        return true;
                    }
                }
                return false;
            }

            public void CopyTo(PropertySpec[] array) {
                innerArray.CopyTo(array);
            }

            public void CopyTo(PropertySpec[] array, int index) {
                innerArray.CopyTo(array, index);
            }

            public IEnumerator GetEnumerator() {
                return innerArray.GetEnumerator();
            }

            public int IndexOf(PropertySpec value) {
                return innerArray.IndexOf(value);
            }

            public int IndexOf(string name) {
                int num = 0;
                foreach (PropertySpec item in innerArray) {
                    if (item.Name == name) {
                        return num;
                    }
                    num++;
                }
                return -1;
            }

            public void Insert(int index, PropertySpec value) {
                innerArray.Insert(index, value);
            }

            public void Remove(PropertySpec obj) {
                innerArray.Remove(obj);
            }

            public void Remove(string name) {
                int index = IndexOf(name);
                RemoveAt(index);
            }

            public void RemoveAt(int index) {
                innerArray.RemoveAt(index);
            }

            public PropertySpec[] ToArray() {
                return (PropertySpec[]) innerArray.ToArray(typeof(PropertySpec));
            }

            void ICollection.CopyTo(Array array, int index) {
                CopyTo((PropertySpec[]) array, index);
            }
        }

        public class PropertySpecDescriptor : PropertyDescriptor {
            public PropertyBag bag;

            public PropertySpec item;

            public override Type ComponentType {
                get {
                    return item.GetType();
                }
            }

            public override Type PropertyType {
                get {
                    return Type.GetType(item.TypeName);
                }
            }

            public override bool IsReadOnly {
                get {
                    return Attributes.Matches(ReadOnlyAttribute.Yes);
                }
            }

            public PropertySpecDescriptor(PropertySpec item, PropertyBag bag, string name, Attribute[] attrs) : base(name, attrs) {
                this.bag = bag;
                this.item = item;
            }

            public override bool CanResetValue(object component) {
                if (item.DefaultValue == null) {
                    return false;
                }
                return !GetValue(component).Equals(item.DefaultValue);
            }

            public override object GetValue(object component) {
                var propertySpecEventArgs = new PropertySpecEventArgs(item, null);
                bag.OnGetValue(propertySpecEventArgs);
                return propertySpecEventArgs.Value;
            }

            public override void ResetValue(object component) {
                SetValue(component, item.DefaultValue);
            }

            public override void SetValue(object component, object value) {
                var e = new PropertySpecEventArgs(item, value);
                bag.OnSetValue(e);
            }

            public override bool ShouldSerializeValue(object component) {
                object value = GetValue(component);
                if (item.DefaultValue == null && value == null) {
                    return false;
                }
                return !value.Equals(item.DefaultValue);
            }
        }

        private string defaultProperty;
        public string DefaultProperty {
            get {
                return defaultProperty;
            }
            set {
                defaultProperty = value;
            }
        }

        private PropertySpecCollection properties;
        public PropertySpecCollection Properties {
            get {
                return properties;
            }
        }

        public event PropertySpecEventHandler GetValue;

        public event PropertySpecEventHandler SetValue;

        public PropertyBag() {
            defaultProperty = null;
            properties = new PropertySpecCollection();
        }

        protected virtual void OnGetValue(PropertySpecEventArgs e) {
            GetValue?.Invoke(this, e);
        }

        protected virtual void OnSetValue(PropertySpecEventArgs e) {
            SetValue?.Invoke(this, e);
        }

        AttributeCollection ICustomTypeDescriptor.GetAttributes() {
            return TypeDescriptor.GetAttributes(this, true);
        }

        string ICustomTypeDescriptor.GetClassName() {
            return TypeDescriptor.GetClassName(this, true);
        }

        string ICustomTypeDescriptor.GetComponentName() {
            return TypeDescriptor.GetComponentName(this, true);
        }

        TypeConverter ICustomTypeDescriptor.GetConverter() {
            return TypeDescriptor.GetConverter(this, true);
        }

        EventDescriptor ICustomTypeDescriptor.GetDefaultEvent() {
            return TypeDescriptor.GetDefaultEvent(this, true);
        }

        PropertyDescriptor ICustomTypeDescriptor.GetDefaultProperty() {
            PropertySpec propertySpec = null;
            if (defaultProperty != null) {
                int index = properties.IndexOf(defaultProperty);
                propertySpec = properties[index];
            }
            if (propertySpec != null) {
                return new PropertySpecDescriptor(propertySpec, this, propertySpec.Name, null);
            }
            return null;
        }

        object ICustomTypeDescriptor.GetEditor(Type editorBaseType) {
            return TypeDescriptor.GetEditor(this, editorBaseType, true);
        }

        EventDescriptorCollection ICustomTypeDescriptor.GetEvents() {
            return TypeDescriptor.GetEvents(this, true);
        }

        EventDescriptorCollection ICustomTypeDescriptor.GetEvents(Attribute[] attributes) {
            return TypeDescriptor.GetEvents(this, attributes, true);
        }

        PropertyDescriptorCollection ICustomTypeDescriptor.GetProperties() {
            return ((ICustomTypeDescriptor) this).GetProperties(new Attribute[0]);
        }

        PropertyDescriptorCollection ICustomTypeDescriptor.GetProperties(Attribute[] attributes) {
            var arrayList = new ArrayList();
            foreach (PropertySpec property in properties) {
                var arrayList2 = new ArrayList();
                if (property.Category != null) {
                    arrayList2.Add(new CategoryAttribute(property.Category));
                }
                if (property.Description != null) {
                    arrayList2.Add(new DescriptionAttribute(property.Description));
                }
                if (property.EditorTypeName != null) {
                    arrayList2.Add(new EditorAttribute(property.EditorTypeName, typeof(UITypeEditor)));
                }
                if (property.ConverterTypeName != null) {
                    arrayList2.Add(new TypeConverterAttribute(property.ConverterTypeName));
                }
                if (property.Attributes != null) {
                    arrayList2.AddRange(property.Attributes);
                }

                var attrs = (Attribute[]) arrayList2.ToArray(typeof(Attribute));
                var value = new PropertySpecDescriptor(property, this, property.Name, attrs);
                arrayList.Add(value);
            }

            var array = (PropertyDescriptor[]) arrayList.ToArray(typeof(PropertyDescriptor));
            return new PropertyDescriptorCollection(array);
        }

        object ICustomTypeDescriptor.GetPropertyOwner(PropertyDescriptor pd) {
            return this;
        }
    }
}
