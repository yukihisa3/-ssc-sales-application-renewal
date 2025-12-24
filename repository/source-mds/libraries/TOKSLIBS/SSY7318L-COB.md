# SSY7318L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY7318L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩオンライン　　　　　　　　　*
*    業務名　　　　　　　：　ダイユーエイト                    *
*    モジュール名　　　　：　ダイユーエイト納品明細書発行      *
*    作成日／更新日　　　：　10/06/22                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    作成日／更新日　　　：　13/09/10                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　日敷追加　　　　　　　　　　　　　*
*    作成日／更新日　　　：　18/01/29                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　リック資材／植物追加（リック専用）*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SSY7318L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/10.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YB-22     IS   BAIKAKU-2-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ダイユーエイト出荷情報ファイル>>--*
     SELECT   DYSYUKF   ASSIGN         DA-01-VI-DYSYUKL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DYE-F01
                                       DYE-F02
                                       DYE-F03
                                       DYE-F04
                                       DYE-F15
                                       DYE-F05
                                       DYE-F08
                                       DYE-F13
                                       DYE-F14
                                       DYE-F06
                                       DYE-F07
                        STATUS         DYSYUKF-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< ダイユーエイト出荷情報ファイル>>--*
 FD  DYSYUKF            LABEL     RECORD   IS   STANDARD.
     COPY     DYSYUKF   OF        XFDLIB
              JOINING   DYE       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03)   VALUE  ZERO.
     03  LINE-CNT1      PIC  9(03)   VALUE  ZERO.
     03  LINE-CNT2      PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT2      PIC  9(03)   VALUE  ZERO.
     03  READ-CNT       PIC  9(08)   VALUE  ZERO.
     03  SET-CNT        PIC  9(02)   VALUE  ZERO.
 01  FLGS.
     03  END-FLG        PIC  X(03)   VALUE  SPACE.
     03  SET-FLG        PIC  X(01)   VALUE  SPACE.
     03  CENTER-FLG     PIC  X(01)   VALUE  SPACE.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DYSYUKF-ST         PIC  X(02).
*
*----<< ｲﾝﾃﾞｯｸｽ >>--*
 01  IX1                PIC  9(02)   VALUE  ZERO.
 01  IX2                PIC  9(02)   VALUE  ZERO.
 01  IX3                PIC  9(02)   VALUE  ZERO.
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
*
 01  WK-SURYO           PIC S9(05)V9(1) VALUE  ZERO.
 01  WK-GENKA           PIC S9(09)V9(2) VALUE  ZERO.
 01  WK-GENKIN          PIC S9(10)V9(2) VALUE  ZERO.
 01  WK-BAIKA           PIC S9(09)V9(2) VALUE  ZERO.
 01  WK-HACSU           PIC S9(07)V9(1) VALUE  ZERO.
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-GK-GENKA    PIC  9(11)      VALUE  ZERO.
     03  WK-GK-BAIKA    PIC  9(11)      VALUE  ZERO.
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  WK-HIZUKE-HENSYU.
     03  WK-H-YYYY      PIC  9(04).
     03  WK-H-KU1       PIC  X(01).
     03  WK-H-MM        PIC  9(02).
     03  WK-H-KU2       PIC  X(01).
     03  WK-H-DD        PIC  9(02).
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  WK-DYE-F01     PIC  9(08).
     03  WK-DYE-F02     PIC  9(04).
     03  WK-DYE-F03     PIC  9(08).
     03  WK-DYE-F04     PIC  X(02).
     03  WK-DYE-F15     PIC  9(08).
     03  WK-DYE-F05     PIC  9(05).
     03  WK-DYE-F08     PIC  9(08).
     03  WK-DYE-F13     PIC  9(04).
     03  WK-DYE-F14     PIC  X(02).
     03  WK-DYE-F06     PIC  9(09).
     03  WK-DYE-F07     PIC  X(02).
     03  WK-DYE-H19     PIC  N(25).
     03  WK-DYE-H04     PIC  9(06).
     03  WK-DYE-H13     PIC  X(13).
     03  WK-DYE-H22     PIC  N(25).
     03  WK-DYE-H15     PIC  N(25).
*
*----<< 部門名称 >>--*
 01  WK-BMONMEI.
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "01".
         05  FILLER     PIC  N(10)
                        VALUE  NC"木材塗料　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "02".
         05  FILLER     PIC  N(10)
                        VALUE  NC"工具金物　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "03".
         05  FILLER     PIC  N(10)
                        VALUE  NC"作業用品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "04".
         05  FILLER     PIC  N(10)
                        VALUE  NC"エクステリア　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "05".
         05  FILLER     PIC  N(10)
                        VALUE  NC"園芸　　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "06".
         05  FILLER     PIC  N(10)
                        VALUE  NC"植物　　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "11".
         05  FILLER     PIC  N(10)
                        VALUE  NC"日用品　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "12".
         05  FILLER     PIC  N(10)
                        VALUE  NC"化粧品　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "13".
         05  FILLER     PIC  N(10)
                        VALUE  NC"家庭用品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "14".
         05  FILLER     PIC  N(10)
                        VALUE  NC"収納用品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "15".
         05  FILLER     PIC  N(10)
                        VALUE  NC"インテリア　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "16".
         05  FILLER     PIC  N(10)
                        VALUE  NC"家電製品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "21".
         05  FILLER     PIC  N(10)
                        VALUE  NC"カーレジャー　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "22".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ペット用品　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "23".
         05  FILLER     PIC  N(10)
                        VALUE  NC"オフィス用品　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "24".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ＯＡ用品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "25".
         05  FILLER     PIC  N(10)
                        VALUE  NC"一般文具　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "26".
         05  FILLER     PIC  N(10)
                        VALUE  NC"サービス　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "27".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ペット（コンセ）　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "31".
         05  FILLER     PIC  N(10)
                        VALUE  NC"医薬品　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "32".
         05  FILLER     PIC  N(10)
                        VALUE  NC"薬用ドリンク　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "33".
         05  FILLER     PIC  N(10)
                        VALUE  NC"健康食品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "41".
         05  FILLER     PIC  N(10)
                        VALUE  NC"調味加工　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "42".
         05  FILLER     PIC  N(10)
                        VALUE  NC"菓子・飲料　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "43".
         05  FILLER     PIC  N(10)
                        VALUE  NC"酒　　　　　　　　　".
*
 01  TBL-BUMONMEI       REDEFINES  WK-BMONMEI.
     03  TBL-MEISYO     OCCURS 25.
         05  TBL-BMNCD  PIC  X(02).
         05  TBL-BMNMEI PIC  N(10).
*
*----<< 部門名称 エイトファーム用  >>--*
 01  WK-BMONMEI1.
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "01".
         05  FILLER     PIC  N(10)
                        VALUE  NC"用品　　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "02".
         05  FILLER     PIC  N(10)
                        VALUE  NC"植物　　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "03".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ハーブ加工　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "04".
         05  FILLER     PIC  N(10)
                        VALUE  NC"鉢・ポット苗　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "08".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ラタンバスケット　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "09".
         05  FILLER     PIC  N(10)
                        VALUE  NC"竹炭　　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "10".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ナチュラルカントリー".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "11".
         05  FILLER     PIC  N(10)
                        VALUE  NC"フレッシュハーブ　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "12".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ドライハーブ　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "13".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ポット苗生産　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "14".
         05  FILLER     PIC  N(10)
                        VALUE  NC"イチゴ生産　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "16".
         05  FILLER     PIC  N(10)
                        VALUE  NC"カフェ　　　　　　　".
*
 01  TBL-BUMONM1           REDEFINES  WK-BMONMEI1.
     03  TBL-MEISYO1       OCCURS 12.
         05  TBL-BMNCD1    PIC  X(02).
         05  TBL-BMNMEI1   PIC  N(10).
*
*----<< 部門名称 >>--*
 01  WK-BMONMEI2.
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "01".
         05  FILLER     PIC  N(10)
                        VALUE  NC"木材塗料　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "02".
         05  FILLER     PIC  N(10)
                        VALUE  NC"工具金物　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "03".
         05  FILLER     PIC  N(10)
                        VALUE  NC"作業資材　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "04".
         05  FILLER     PIC  N(10)
                        VALUE  NC"エクステリア　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "05".
         05  FILLER     PIC  N(10)
                        VALUE  NC"園芸　　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "06".
         05  FILLER     PIC  N(10)
                        VALUE  NC"植物　　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "07".
         05  FILLER     PIC  N(10)
                        VALUE  NC"作業衣料　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "08".
         05  FILLER     PIC  N(10)
                        VALUE  NC"農業資材　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "11".
         05  FILLER     PIC  N(10)
                        VALUE  NC"日用品　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "12".
         05  FILLER     PIC  N(10)
                        VALUE  NC"化粧品　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "13".
         05  FILLER     PIC  N(10)
                        VALUE  NC"家庭用品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "14".
         05  FILLER     PIC  N(10)
                        VALUE  NC"収納用品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "15".
         05  FILLER     PIC  N(10)
                        VALUE  NC"インテリア　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "16".
         05  FILLER     PIC  N(10)
                        VALUE  NC"家電製品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "20".
         05  FILLER     PIC  N(10)
                        VALUE  NC"サイクル　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "21".
         05  FILLER     PIC  N(10)
                        VALUE  NC"カーレジャー　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "22".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ペット用品　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "23".
         05  FILLER     PIC  N(10)
                        VALUE  NC"オフィス用品　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "24".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ＯＡ用品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "25".
         05  FILLER     PIC  N(10)
                        VALUE  NC"一般文具　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "26".
         05  FILLER     PIC  N(10)
                        VALUE  NC"サービス　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "27".
         05  FILLER     PIC  N(10)
                        VALUE  NC"ペット（コンセ）　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "31".
         05  FILLER     PIC  N(10)
                        VALUE  NC"医薬品　　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "32".
         05  FILLER     PIC  N(10)
                        VALUE  NC"薬用ドリンク　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "33".
         05  FILLER     PIC  N(10)
                        VALUE  NC"健康食品　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "41".
         05  FILLER     PIC  N(10)
                        VALUE  NC"調味加工　　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "42".
         05  FILLER     PIC  N(10)
                        VALUE  NC"菓子・飲料　　　　　".
     03  FILLER.
         05  FILLER     PIC  X(02)  VALUE  "43".
         05  FILLER     PIC  N(10)
                        VALUE  NC"酒　　　　　　　　　".
*
 01  TBL-BUMONMEI2      REDEFINES  WK-BMONMEI2.
     03  TBL-MEISYO2    OCCURS 25.
         05  TBL-BMNCD2   PIC  X(02).
         05  TBL-BMNMEI2  PIC  N(10).
*
 01  LST-DATA.
     03  LIT-DATA1            OCCURS 9.
         05  LST-DENNO        PIC  X(09).
         05  LST-GYONO        PIC  ZZ9.
         05  LST-JANCDX.
             07  LST-JANCD    PIC  X(13).
             07  FILLER       PIC  X(01).
         05  LST-SHOME        PIC  N(26).
         05  LST-HACSU        PIC  9(09)V9(01).
         05  LST-NOHSU        PIC  9(09)V9(01).
         05  LST-GENTAN       PIC  9(09)V9(02).
         05  LST-GENKIN       PIC  9(09).
         05  LST-BAITAN       PIC  9(09).
         05  LST-BAIKIN       PIC  9(10).
         05  LST-RIYUUX.
             07  FILLER       PIC  X(01).
             07  LST-RIYUU    PIC  X(03).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
*#2018/01/31 NAV ST
*********05  HD00-KKME  PIC  N(13).
*********05  FILLER     PIC  X(86)    VALUE  SPACE.
         05  HD00-KKME  PIC  N(15).
         05  FILLER     PIC  X(82)    VALUE  SPACE.
*#2018/01/31 NAV ED
         05  HD00-YYYY  PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD00-MM    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD00-DD    PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(05)     VALUE  "PAGE-".
         05  HD00-PAGE  PIC  ZZ9.
*
 01  HD01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER        PIC  X(62)     VALUE  SPACE.
         05  HD01-DENTYPE  PIC  N(05).
*
 01  HD02.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  HD02-HACYY PIC  9999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD02-HACMM PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD02-HACDD PIC  Z9.
         05  FILLER     PIC  X(13)     VALUE  SPACE.
         05  HD02-TENCD PIC  ZZZZZZZZZZZZ9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD02-TENME PIC  N(10).
         05  FILLER     PIC  X(17)     VALUE  SPACE.
         05  HD02-TORCD PIC  999999.
*
 01  HD03.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  HD03-NOHYY PIC  9999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-NOHMM PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  HD03-NOHDD PIC  Z9.
         05  FILLER     PIC  X(24)     VALUE  SPACE.
         05  HD03-BUMCD PIC  99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD03-BUMME PIC  N(10).
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  HD03-TORME PIC  N(10).
*
 01  HD04.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  HD04-DENKU PIC  99.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD04-DENNM PIC  N(05).
*
 01  MS01.
     03  FILLER            CHARACTER  TYPE  PITCH-1-5.
         05  FILLER        PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO    PIC  XXXXXXXXX.
         05  MS01-GYONO    PIC  ZZ9.
         05  MS01-JANCD    PIC  X(14)     VALUE  SPACE.
         05  MS01-SHOME    PIC  N(26).
         05  MS01-HACSU    PIC  ZZZ,ZZ9.9.
         05  MS01-NOHSU    PIC  ZZZ,ZZ9.9.
         05  MS01-TEISEI   PIC  X(06)     VALUE  SPACE.
         05  FILLER        PIC  X(01).
         05  MS01-GENTAN   PIC  ZZZ,ZZ9.99.
         05  MS01-GENKIN   PIC  ZZZ,ZZZ,ZZ9.
         05  MS01-BAITAN   PIC  Z,ZZZ,ZZ9.
         05  MS01-BAIKIN   PIC  ZZZ,ZZZ,ZZ9.
         05  FILLER        PIC  X(01)     VALUE  SPACE.
         05  MS01-RIYUU    PIC  X(02)     VALUE  SPACE.
*
 01  GK01.
     03  FILLER            CHARACTER TYPE PITCH-1-5.
         05  FILLER        PIC  X(01)     VALUE  SPACE.
         05  FILLER        PIC  X(35)     VALUE  SPACE.
         05  FILLER        PIC  X(01)     VALUE  SPACE.
         05  GK01-GKMEI    PIC  N(10)
                           VALUE  NC"伝　　票　　合　　計".
         05  FILLER        PIC  X(01)     VALUE  SPACE.
         05  FILLER        PIC  X(48)     VALUE  SPACE.
         05  GK01-GENGOK   PIC  ZZZ,ZZZ,ZZ9.
         05  FILLER        PIC  X(09)     VALUE  SPACE.
         05  GK01-BAIGOK   PIC  ZZZ,ZZZ,ZZ9.
         05  FILLER        PIC  X(03)     VALUE  SPACE.
*2018/02/13 NAV ST リックの場合、センター名を印字
 01  GK02.
     03  FILLER            CHARACTER TYPE PITCH-1-5.
         05  FILLER        PIC  X(100)    VALUE  SPACE.
         05  GK02-CENTER   PIC  N(10).
*
 01  MSG-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE     SECTION.
 01  LINK-JDATE             PIC  9(08).
 01  LINK-JTIME             PIC  9(04).
 01  LINK-TORICD            PIC  9(08).
 01  LINK-SOKO              PIC  X(02).
 01  LINK-STENCD            PIC  9(05).
 01  LINK-ETENCD            PIC  9(05).
 01  LINK-SNOUDT            PIC  9(08).
 01  LINK-ENOUDT            PIC  9(08).
 01  LINK-SDENNO            PIC  9(09).
 01  LINK-EDENNO            PIC  9(09).
****************************************************************
 PROCEDURE              DIVISION  USING  LINK-JDATE
                                         LINK-JTIME
                                         LINK-TORICD
                                         LINK-SOKO
                                         LINK-STENCD
                                         LINK-ETENCD
                                         LINK-SNOUDT
                                         LINK-ENOUDT
                                         LINK-SDENNO
                                         LINK-EDENNO.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ダイユーエイト出荷情報ファイル >>--*
 DYSYUKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DYSYUKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY7318L DYSYUKF ERROR " DYSYUKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    DYSYUKF.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-TIME       FROM TIME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     DISPLAY  "*** SSY7318L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     DYSYUKF.
     OPEN     OUTPUT    PRTF.
*
     INITIALIZE  BREAK-KEY.
*ダイユーエイト出荷確定データＲＥＡＤ
     MOVE     SPACE               TO   DYE-REC.
     INITIALIZE                        DYE-REC.
     MOVE     LINK-JDATE          TO   DYE-F01.
     MOVE     LINK-JTIME          TO   DYE-F02.
     MOVE     LINK-TORICD         TO   DYE-F03.
     MOVE     LINK-SOKO           TO   DYE-F04.
     MOVE     LINK-STENCD         TO   DYE-F05.
     MOVE     LINK-SNOUDT         TO   DYE-F08.
     MOVE     LINK-SDENNO         TO   DYE-F06.
     PERFORM  900-DYE-START-READ.
*
     IF       END-FLG  =  "END"
              DISPLAY NC"＃＃出力対象無し＃＃" UPON CONS
              STOP  RUN
     ELSE
              MOVE     DYE-F01    TO   WK-DYE-F01
              MOVE     DYE-F02    TO   WK-DYE-F02
              MOVE     DYE-F03    TO   WK-DYE-F03
              MOVE     DYE-F04    TO   WK-DYE-F04
              MOVE     DYE-F15    TO   WK-DYE-F15
              MOVE     DYE-F08    TO   WK-DYE-F08
              MOVE     DYE-F13    TO   WK-DYE-F13
              MOVE     DYE-F14    TO   WK-DYE-F14
              MOVE     DYE-F06    TO   WK-DYE-F06
              MOVE     DYE-F07    TO   WK-DYE-F07
              MOVE     DYE-H19    TO   WK-DYE-H19
              MOVE     DYE-H04    TO   WK-DYE-H04
              MOVE     DYE-H13    TO   WK-DYE-H13
              MOVE     DYE-H22    TO   WK-DYE-H22
              MOVE     DYE-H15    TO   WK-DYE-H15
              MOVE     99         TO   LINE-CNT
              MOVE     ZERO       TO   PAGE-CNT
              MOVE     ZERO       TO   IX1
              MOVE     1          TO   IX2
              MOVE     ZERO       TO   IX3
              MOVE     SPACE      TO   CENTER-FLG
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*    バッチ日付／バッチ時刻／バッチ取引先／倉庫ＣＤ／納品日／
*    発注日／店舗ＣＤ／部門ＣＤ／伝票タイプ／伝票区分／ルート
*    ブレイク時改ページ
     IF       DYE-F01   NOT =  WK-DYE-F01
     OR       DYE-F02   NOT =  WK-DYE-F02
     OR       DYE-F03   NOT =  WK-DYE-F03
     OR       DYE-F04   NOT =  WK-DYE-F04
     OR       DYE-F15   NOT =  WK-DYE-F15
     OR       DYE-F05   NOT =  WK-DYE-F05
     OR       DYE-F08   NOT =  WK-DYE-F08
     OR       DYE-F13   NOT =  WK-DYE-F13
     OR       DYE-F14   NOT =  WK-DYE-F14
              IF    SET-CNT       >  ZERO
*              明細行出力
                    PERFORM  MEISAI-WT-SEC
                       VARYING IX3 FROM 1 BY 1
                       UNTIL  IX3     >  SET-CNT
                    ADD       SET-CNT     TO   LINE-CNT
*              合計行出力
                    PERFORM  GOKEI-WT-SEC
********************ADD       1           TO   LINE-CNT
                    ADD       2           TO   LINE-CNT
              END-IF
              MOVE   SPACE        TO  LST-DATA
              INITIALIZE              LST-DATA
              MOVE     1          TO  IX2
              MOVE     ZERO       TO  SET-CNT
              MOVE     ZERO       TO  PAGE-CNT2
*             見出し出力
*             ブレイクキー設定
              MOVE      DYE-F01   TO  WK-DYE-F01
              MOVE      DYE-F02   TO  WK-DYE-F02
              MOVE      DYE-F03   TO  WK-DYE-F03
              MOVE      DYE-F04   TO  WK-DYE-F04
              MOVE      DYE-F15   TO  WK-DYE-F15
              MOVE      DYE-F05   TO  WK-DYE-F05
              MOVE      DYE-F08   TO  WK-DYE-F08
              MOVE      DYE-F13   TO  WK-DYE-F13
              MOVE      DYE-F14   TO  WK-DYE-F14
              MOVE      DYE-H19   TO  WK-DYE-H19
              MOVE      DYE-H04   TO  WK-DYE-H04
              MOVE      DYE-H13   TO  WK-DYE-H13
              MOVE      DYE-H22   TO  WK-DYE-H22
              MOVE      DYE-H15   TO  WK-DYE-H15
              PERFORM   HEAD-WT-SEC
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    伝票番号がブレイク
     IF       DYE-F06   NOT =  WK-DYE-F06
*          明細データを出力
              MOVE   1            TO  IX3
              PERFORM  MEISAI-WT-SEC
                       VARYING IX3 FROM 1 BY 1
                       UNTIL  IX3     >  SET-CNT
              ADD       SET-CNT     TO   LINE-CNT
              MOVE   SPACE        TO  LST-DATA
              INITIALIZE              LST-DATA
              IF  PAGE-CNT > ZERO AND
                  SET-CNT  > ZERO
                  PERFORM  GOKEI-WT-SEC
******************ADD       1           TO   LINE-CNT
                  ADD       2           TO   LINE-CNT
              END-IF
              MOVE      1         TO  IX2
              MOVE      ZERO      TO  IX3
              MOVE      ZERO      TO  SET-CNT
              MOVE      DYE-F01   TO  WK-DYE-F01
              MOVE      DYE-F02   TO  WK-DYE-F02
              MOVE      DYE-F03   TO  WK-DYE-F03
              MOVE      DYE-F04   TO  WK-DYE-F04
              MOVE      DYE-F15   TO  WK-DYE-F15
              MOVE      DYE-F05   TO  WK-DYE-F05
              MOVE      DYE-F08   TO  WK-DYE-F08
              MOVE      DYE-F13   TO  WK-DYE-F13
              MOVE      DYE-F14   TO  WK-DYE-F14
              MOVE      DYE-F06   TO  WK-DYE-F06
              MOVE      DYE-F07   TO  WK-DYE-F07
              MOVE      DYE-H19   TO  WK-DYE-H19
              MOVE      DYE-H04   TO  WK-DYE-H04
              MOVE      DYE-H13   TO  WK-DYE-H13
              MOVE      DYE-H22   TO  WK-DYE-H22
              MOVE      DYE-H15   TO  WK-DYE-H15
              MOVE      ZERO      TO  WK-GK-GENKA WK-GK-BAIKA
     END-IF.
*    明細データ退避
     PERFORM  MEISAI-BODY-SEC.
*    ダイユーエイト出荷確定データ読込み
     PERFORM  900-DYE-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
*
     IF    SET-CNT       >  ZERO
*    明細行出力
           PERFORM  MEISAI-WT-SEC
                    VARYING IX3 FROM 1 BY 1
                    UNTIL  IX3     >  SET-CNT
           ADD       SET-CNT     TO   LINE-CNT
*    合計行出力
           PERFORM  GOKEI-WT-SEC
***********ADD       SET-CNT     TO   LINE-CNT
           ADD       2           TO   LINE-CNT
***********DISPLAY "CENTER-FLG2 = " CENTER-FLG UPON CONS
           IF  CENTER-FLG = "1"
***************DISPLAY "LINE-CNT2 = " LINE-CNT UPON CONS
               MOVE NC"早島センター"  TO  GK02-CENTER
               COMPUTE LINE-CNT2 = 58 - LINE-CNT
               WRITE    PRT-REC  FROM  GK02  AFTER  LINE-CNT2
           END-IF
     END-IF.
*
     DISPLAY "ﾀｲｼｮｳDT CNT = " READ-CNT  UPON CONS.
     DISPLAY NC"＃総出力枚数⇒" " " PAGE-CNT NC"枚" UPON CONS.
*
     CLOSE    DYSYUKF  PRTF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY7318L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              IF  CENTER-FLG = "1"
                  MOVE NC"早島センター"  TO  GK02-CENTER
***************DISPLAY "LINE-CNT1 = " LINE-CNT UPON CONS
                  COMPUTE LINE-CNT2 = 58 - LINE-CNT
                  WRITE    PRT-REC  FROM  GK02  AFTER  LINE-CNT2
                  MOVE     SPACE         TO  CENTER-FLG
              END-IF
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO      TO        LINE-CNT.
*    頁カウンター
     ADD      1         TO        PAGE-CNT  PAGE-CNT2.
*    MOVE     PAGE-CNT  TO        HD00-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4) TO   WK-H-YYYY.
     MOVE     SYS-DATEW(5:2) TO   WK-H-MM.
     MOVE     SYS-DATEW(7:2) TO   WK-H-DD.
     MOVE     "/"            TO   WK-H-KU1 WK-H-KU2.
     MOVE     WK-H-YYYY      TO   HD00-YYYY.
     MOVE     WK-H-MM        TO   HD00-MM.
     MOVE     WK-H-DD        TO   HD00-DD.
     MOVE     PAGE-CNT       TO   HD00-PAGE.
*    小売企業名称（漢字）
     MOVE     WK-DYE-H15         TO  HD00-KKME.
*    伝票タイプ　
     EVALUATE WK-DYE-F14
         WHEN "00"
         WHEN "01"
         WHEN "02"
         WHEN "03"
         WHEN "04"
         WHEN "05"
         WHEN "06"
              MOVE NC"ＥＯＳ"       TO  HD01-DENTYPE
         WHEN "07"
              MOVE NC"ダイレクト"   TO  HD01-DENTYPE
     END-EVALUATE.
*    発注日
     MOVE     WK-DYE-F15(1:4)   TO   HD02-HACYY.
     MOVE     WK-DYE-F15(5:2)   TO   HD02-HACMM.
     MOVE     WK-DYE-F15(7:2)   TO   HD02-HACDD.
*    店ＣＤ
     MOVE     WK-DYE-F05        TO   HD02-TENCD.
*    店舗名称
     MOVE     WK-DYE-H19        TO   HD02-TENME.
*    取引先
*****MOVE     DYE-F03        TO   HD02-TORCD.
     MOVE     WK-DYE-H04        TO   HD02-TORCD.
*    納品日
     MOVE     WK-DYE-F08(1:4)   TO   HD03-NOHYY.
     MOVE     WK-DYE-F08(5:2)   TO   HD03-NOHMM.
     MOVE     WK-DYE-F08(7:2)   TO   HD03-NOHDD.
*    部門　
     MOVE     WK-DYE-F13        TO   HD03-BUMCD.
*    部門名称テーブルより名称を取得し設定
     MOVE     SPACE          TO   SET-FLG.
     PERFORM  VARYING  IX1 FROM 1 BY 1
                           UNTIL  IX1     > 25  OR
                                  SET-FLG = "1"
       IF   WK-DYE-H13(1:4)  =  "4950"
         IF   TBL-BMNCD(IX1)  =   WK-DYE-F13(3:2)
              MOVE  TBL-BMNMEI(IX1)   TO   HD03-BUMME
              MOVE  "1"               TO   SET-FLG
         END-IF
       END-IF
*
       IF   WK-DYE-H13(1:4)  =  "4952"
         IF   TBL-BMNCD1(IX1)  =   WK-DYE-F13(3:2)
              MOVE  TBL-BMNMEI1(IX1)  TO   HD03-BUMME
              MOVE  "1"               TO   SET-FLG
         END-IF
       END-IF
*2013.09.10↓
       IF   WK-DYE-H13(1:4)  =  "4953"
         IF   TBL-BMNCD(IX1)  =   WK-DYE-F13(3:2)
              MOVE  TBL-BMNMEI(IX1)   TO   HD03-BUMME
              MOVE  "1"               TO   SET-FLG
         END-IF
       END-IF
*2013.09.10↑
*2018.01.29↓
       IF   WK-DYE-H13(1:4)  =  "4978"
         IF   TBL-BMNCD2(IX1)  =   WK-DYE-F13(3:2)
              MOVE  TBL-BMNMEI2(IX1)  TO   HD03-BUMME
              MOVE  "1"               TO   SET-FLG
         END-IF
       END-IF
*2018.01.29↑
     END-PERFORM.
*
     IF  SET-FLG  =  SPACE
         MOVE     SPACE      TO   HD03-BUMME
     END-IF.
*取引先名
     MOVE     WK-DYE-H22        TO   HD03-TORME.
*伝票区分
     MOVE     WK-DYE-F14        TO   HD04-DENKU.
     EVALUATE WK-DYE-F14
         WHEN "00"
              MOVE NC"定番"        TO  HD04-DENNM
         WHEN "01"
              MOVE NC"合同"        TO  HD04-DENNM
         WHEN "04"
              MOVE NC"入替"        TO  HD04-DENNM
         WHEN "05"
              MOVE NC"特売"        TO  HD04-DENNM
         WHEN "07"
              MOVE NC"ダイレクト"  TO  HD04-DENNM
         WHEN "50"
              MOVE NC"仕入"        TO  HD04-DENNM
         WHEN "51"
              MOVE NC"返品"        TO  HD04-DENNM
         WHEN "52"
              MOVE NC"値引"        TO  HD04-DENNM
     END-EVALUATE.
*    ヘッダ印刷
     WRITE    PRT-REC  FROM  HD00  AFTER  1.
     WRITE    PRT-REC  FROM  HD01  AFTER  1.
     WRITE    PRT-REC  FROM  HD02  AFTER  2.
     WRITE    PRT-REC  FROM  HD03  AFTER  1.
     WRITE    PRT-REC  FROM  HD04  AFTER  1.
     MOVE     SPACE                TO  PRT-REC.
     WRITE    PRT-REC              AFTER  3.
*行カウント
     MOVE     10             TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    出荷情報ファイル退避
*--------------------------------------------------------------*
 MEISAI-BODY-SEC        SECTION.
     MOVE     "MEISAI-BODY-SEC"   TO   S-NAME.
*    伝票番号
     MOVE   DYE-F06               TO   LST-DENNO(IX2).
*****リックセンター発注分の伝票番号の場合
     IF ( WK-DYE-H04 = 1994  OR  1995 )
     AND  DYE-F06   >=  5000001
     AND  DYE-F06   <=  8999999
          MOVE   "1"              TO   CENTER-FLG
     END-IF.
*****DISPLAY "CENTER-FLG1 = " CENTER-FLG UPON CONS.
*    行
     MOVE   DYE-F07               TO   LST-GYONO(IX2).
*    ＪＡＮＣＤ
     MOVE   DYE-M02               TO   LST-JANCDX(IX2).
*    商品名
     MOVE   DYE-M06               TO   LST-SHOME(IX2).
*    発注数量
     COMPUTE    WK-HACSU   =   DYE-M07  /  100.
     MOVE   WK-HACSU              TO   LST-HACSU(IX2).
*    納品数量
*****MOVE   DYE-M09A              TO   LST-NOHSU(IX2).
     COMPUTE  LST-NOHSU(IX2)  =  DYE-M09A  /  10.
*    原単価
     COMPUTE    WK-GENKA   =   DYE-M12  /   100.
     MOVE   WK-GENKA              TO   LST-GENTAN(IX2).
*    売単価
**** COMPUTE    WK-BAIKA   =   DYE-M13  /   100.
     MOVE   DYE-M13               TO   LST-BAITAN(IX2).
*    原価金額
     COMPUTE  WK-GENKIN  =   DYE-M10  /  100.
     MOVE   WK-GENKIN             TO   LST-GENKIN(IX2).
*    売価金額
     MOVE   DYE-M11               TO   LST-BAIKIN(IX2).
*    欠品理由
     MOVE   DYE-M14A              TO   LST-RIYUU(IX2).
*
     MOVE   IX2                   TO   SET-CNT.
     ADD    1                     TO   IX2.
*
 MEISAI-BODY-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細ボディー印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC         SECTION.
     MOVE     "MEISAI-BODY-SEC"   TO   S-NAME.
*    ヘッダ出力判定
     COMPUTE  LINE-CNT1  =   LINE-CNT  +  SET-CNT + 1.
     IF     LINE-CNT1   >  57
            PERFORM   HEAD-WT-SEC
     END-IF.
*    レコードクリア
     MOVE   SPACE                 TO      MS01   PRT-REC.
     INITIALIZE                           MS01   PRT-REC.
*
*    伝票番号
     IF     IX3   =   1
            MOVE   LST-DENNO(IX3)    TO   MS01-DENNO
     END-IF.
*    行
     MOVE   LST-GYONO(IX3)    TO   MS01-GYONO
*    ＪＡＮＣＤ
*****MOVE   LST-JANCD(IX3)    TO   MS01-JANCD
     MOVE   LST-JANCDX(IX3)   TO   MS01-JANCD
*    商品名
     MOVE   LST-SHOME(IX3)    TO   MS01-SHOME
*    発注数量
     MOVE   LST-HACSU(IX3)    TO   MS01-HACSU.
*    納品数量
     MOVE   LST-NOHSU(IX3)    TO   MS01-NOHSU.
*    訂正数量
     MOVE   "(    )"             TO   MS01-TEISEI.
*    原価
     MOVE   LST-GENTAN(IX3)      TO   MS01-GENTAN.
*    売価
     MOVE   LST-BAITAN(IX3)      TO   MS01-BAITAN.
*    原価金額
     MOVE   LST-GENKIN(IX3)      TO   MS01-GENKIN.
*    売価金額
     MOVE   LST-BAIKIN(IX3)      TO   MS01-BAIKIN.
*    欠品理由
     MOVE   LST-RIYUU(IX3)       TO   MS01-RIYUU.
*
     ADD    LST-GENKIN(IX3)      TO   WK-GK-GENKA.
*    DISPLAY  "LST-GENKIN =" LST-GENKIN(IX3) UPON CONS.
*    DISPLAY  "WK-GK-GENKA=" WK-GK-GENKA UPON CONS.
     ADD    LST-BAIKIN(IX3)      TO   WK-GK-BAIKA.
*    DISPLAY  "LST-BAIKIN =" LST-BAIKIN(IX3) UPON CONS.
*    DISPLAY  "WK-GK-BAIKA=" WK-GK-BAIKA UPON CONS.
*
     WRITE     PRT-REC   FROM  MS01  AFTER  1.
*    ADD       1           TO   LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細合計印字
*--------------------------------------------------------------*
 GOKEI-WT-SEC                 SECTION.
     MOVE     "GOKEI-WT-SEC" TO   S-NAME.
*    原価金額合計
     MOVE     WK-GK-GENKA       TO    GK01-GENGOK.
*    売価金額合計
     MOVE     WK-GK-BAIKA       TO    GK01-BAIGOK.
*
     WRITE     PRT-REC   FROM  GK01  AFTER  1.
*
     MOVE      SPACE            TO    PRT-REC.
     WRITE     PRT-REC               AFTER  1.
*
     MOVE     ZERO              TO    WK-GK-GENKA WK-GK-BAIKA.
*
*    ADD       1           TO   LINE-CNT.
*
 GOKEI-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DYE-START-READ     SECTION.
     MOVE     "900-DYE-START-READ"     TO   S-NAME.
     START    DYSYUKF   KEY  >=   DYE-F01  DYE-F02
                                  DYE-F03  DYE-F04
                                  DYE-F15  DYE-F05
                                  DYE-F08  DYE-F13
                                  DYE-F14  DYE-F06
                                  DYE-F07
              INVALID   KEY
**********    DISPLAY "AAA" UPON CONS
              MOVE     "END"      TO   END-FLG
              GO                  TO   900-DYE-START-READ-EXIT
     END-START.
*
     PERFORM   900-DYE-READ.
*
 900-DYE-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ダイユーエイト出荷確定ファイル　 READ
*--------------------------------------------------------------*
 900-DYE-READ           SECTION.
     MOVE     "900-DYE-READ"      TO   S-NAME.
*
     READ     DYSYUKF   AT   END
**********    DISPLAY "DDD" UPON CONS
              MOVE     "END"      TO   END-FLG
              GO        TO        900-DYE-READ-EXIT
     END-READ.
*店別データ以外（総量）読飛
     IF   DYE-H12   NOT = 1
          GO   TO   900-DYE-READ.
*
*ブレイクチェック
     IF   LINK-SOKO = SPACE THEN
          IF       DYE-F01   NOT =  LINK-JDATE       OR
                   DYE-F02   NOT =  LINK-JTIME       OR
                   DYE-F03   NOT =  LINK-TORICD
**************DISPLAY "BBB" UPON CONS
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-DYE-READ-EXIT
          END-IF
     ELSE
          IF       DYE-F01   NOT =  LINK-JDATE       OR
                   DYE-F02   NOT =  LINK-JTIME       OR
                   DYE-F03   NOT =  LINK-TORICD      OR
                   DYE-F04   NOT =  LINK-SOKO
**************DISPLAY "CCC" UPON CONS
                   MOVE     "END"      TO   END-FLG
                   GO        TO        900-DYE-READ-EXIT
          END-IF
     END-IF.
*店舗ＣＤ範囲チェック
     IF       LINK-STENCD  <=  DYE-F05
     AND      LINK-ETENCD  >=  DYE-F05
              CONTINUE
     ELSE
**************DISPLAY "EEE" UPON CONS
              GO        TO        900-DYE-READ
     END-IF.
*伝票区分によるデータ抽出チェック
     IF       DYE-F14  >=  "00"  AND
              DYE-F14  <=  "05"  OR
              DYE-F14   =  "07"
              CONTINUE
     ELSE
**************DISPLAY "EEE" UPON CONS
              GO        TO        900-DYE-READ
     END-IF.
*
*納品日範囲チェック
     IF       LINK-SNOUDT  <=  DYE-F08
     AND      LINK-ENOUDT  >=  DYE-F08
              CONTINUE
     ELSE
**************DISPLAY "FFF" UPON CONS
              GO        TO        900-DYE-READ
     END-IF.
*
*伝票ＮＯ範囲チェック
     IF       LINK-SDENNO  <=  DYE-F06
     AND      LINK-EDENNO  >=  DYE-F06
              CONTINUE
     ELSE
              GO        TO        900-DYE-READ
     END-IF.
*
     ADD      1         TO        READ-CNT.
*
 900-DYE-READ-EXIT.
     EXIT.

```
