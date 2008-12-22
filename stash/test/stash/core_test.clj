(ns stash.core-test
  (:use clj-unit.core
        stash.core
        stash.utils
        clj-time.core)
  (:load "core_test_helper" "core_column_mappings_test" "core_def_test"
         "core_uuid_test" "core_callbacks_test" "core_validations_test"
         "core_crud_test" "core_finders_test"))