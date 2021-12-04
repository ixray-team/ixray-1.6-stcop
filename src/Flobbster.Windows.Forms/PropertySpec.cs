using System;

namespace Flobbster.Windows.Forms {
    public class PropertySpec {
        private Attribute[] attributes;
        public Attribute[] Attributes {
            get {
                return attributes;
            }
            set {
                attributes = value;
            }
        }

        private string category;
        public string Category {
            get {
                return category;
            }
            set {
                category = value;
            }
        }

        private string typeConverter;
        public string ConverterTypeName {
            get {
                return typeConverter;
            }
            set {
                typeConverter = value;
            }
        }

        private object defaultValue;
        public object DefaultValue {
            get {
                return defaultValue;
            }
            set {
                defaultValue = value;
            }
        }

        private string description;
        public string Description {
            get {
                return description;
            }
            set {
                description = value;
            }
        }

        private string editor;
        public string EditorTypeName {
            get {
                return editor;
            }
            set {
                editor = value;
            }
        }

        private string name;
        public string Name {
            get {
                return name;
            }
            set {
                name = value;
            }
        }

        private string type;
        public string TypeName {
            get {
                return type;
            }
            set {
                type = value;
            }
        }

        public PropertySpec(string name, string type, string category, string description, object defaultValue) {
            this.name = name;
            this.type = type;
            this.category = category;
            this.description = description;
            this.defaultValue = defaultValue;
            attributes = null;
        }

        public PropertySpec(string name, string type, string category, string description, object defaultValue, string editor, string typeConverter) : this(name, type, category, description, defaultValue) {
            this.editor = editor;
            this.typeConverter = typeConverter;
        }

        public PropertySpec(string name, string type) : this(name, type, null, null, null) {
        }

        public PropertySpec(string name, Type type) : this(name, type.AssemblyQualifiedName, null, null, null) {
        }

        public PropertySpec(string name, string type, string category) : this(name, type, category, null, null) {
        }

        public PropertySpec(string name, Type type, string category) : this(name, type.AssemblyQualifiedName, category, null, null) {
        }

        public PropertySpec(string name, string type, string category, string description) : this(name, type, category, description, null) {
        }

        public PropertySpec(string name, Type type, string category, string description) : this(name, type.AssemblyQualifiedName, category, description, null) {
        }

        public PropertySpec(string name, Type type, string category, string description, object defaultValue) : this(name, type.AssemblyQualifiedName, category, description, defaultValue) {
        }

        public PropertySpec(string name, Type type, string category, string description, object defaultValue, string editor, string typeConverter) : this(name, type.AssemblyQualifiedName, category, description, defaultValue, editor, typeConverter) {
        }

        public PropertySpec(string name, string type, string category, string description, object defaultValue, Type editor, string typeConverter) : this(name, type, category, description, defaultValue, editor.AssemblyQualifiedName, typeConverter) {
        }

        public PropertySpec(string name, Type type, string category, string description, object defaultValue, Type editor, string typeConverter) : this(name, type.AssemblyQualifiedName, category, description, defaultValue, editor.AssemblyQualifiedName, typeConverter) {
        }

        public PropertySpec(string name, string type, string category, string description, object defaultValue, string editor, Type typeConverter) : this(name, type, category, description, defaultValue, editor, typeConverter.AssemblyQualifiedName) {
        }

        public PropertySpec(string name, Type type, string category, string description, object defaultValue, string editor, Type typeConverter) : this(name, type.AssemblyQualifiedName, category, description, defaultValue, editor, typeConverter.AssemblyQualifiedName) {
        }

        public PropertySpec(string name, string type, string category, string description, object defaultValue, Type editor, Type typeConverter) : this(name, type, category, description, defaultValue, editor.AssemblyQualifiedName, typeConverter.AssemblyQualifiedName) {
        }

        public PropertySpec(string name, Type type, string category, string description, object defaultValue, Type editor, Type typeConverter) : this(name, type.AssemblyQualifiedName, category, description, defaultValue, editor.AssemblyQualifiedName, typeConverter.AssemblyQualifiedName) {
        }
    }
}
