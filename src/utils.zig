const std = @import("std");

pub fn isStructOrUnion(comptime T: type) bool {
    return isStruct(T) or isUnion(T);
}

pub fn isStruct(comptime T: type) bool {
    return @typeInfo(T) == .Struct;
}

pub fn isUnion(comptime T: type) bool {
    return @typeInfo(T) == .Union;
}
