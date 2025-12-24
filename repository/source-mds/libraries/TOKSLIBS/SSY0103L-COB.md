# SSY0103L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY0103L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　手書きチェックリスト　　　　　　　*
*    作成日／更新日　　　：　99/09/13                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　伝票データより，手書きチェック　　*
*                        ：　リストを作成する。　　　　　　　　*
*                            10/01/05仕入単価出力、返品廃棄伝票*
*                            のみ、の追加　                    *
*    更新日／更新者　　　：　11/10/06 / YOSHIDA.M              *
*    更新概要　　　　　　：　基幹サーバ統合                    *
*    更新日／更新者　　　：　14/01/06 / INOUE                  *
*    更新概要　　　　　　：　消費税対応　　                    *
*    更新日／更新者　　　：　14/01/23 / TAKAHASHI              *
*    更新概要　　　　　　：　消費税対応（税区分名出力条件変更）*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY0103L.
 AUTHOR.                NAV Y.YOSHIDA.
 DATE-WRITTEN.          99/09/13.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 伝票データ >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLH
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F63   DEN-F60
                                       DEN-F01   DEN-F02
                                       DEN-F04   DEN-F051
***2011.10.06(DEN-F07,DEN-F112)
                                       DEN-F07   DEN-F112
                                       DEN-F03
                        STATUS         SHTDENF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         HTENMS-ST.
*2008/07/28)NAV 後閑
*----<< 担当者マスタ >>--*
     SELECT   HTANMS    ASSIGN         DA-01-VI-TANMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TAN-F01   TAN-F02
                        STATUS         HTANMS-ST.
*2014/01/06↓
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-ST.
*2014/01/06↑
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY01022  OF        XMDLIB.
*----<< 伝票データ >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*2008/07/29)NAV 後閑
*----<< 担当者マスタ >>--*
 FD  HTANMS             LABEL RECORD   IS   STANDARD.
     COPY     HTANMS    OF        XFDLIB
              JOINING   TAN       PREFIX.
*2014/01/06↓
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*2014/01/06↑
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  SKIP-FLG       PIC  9(01)  VALUE  ZERO.
     03  HTENMS-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  HTANMS-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  AUTO-FLG       PIC  X(01)  VALUE  SPACE.
*2014/01/06↓
 01  JYO-INVALID-FLG       PIC  9(01) VALUE ZERO.
 01  WK-TOK-F19            PIC  9(11) VALUE ZERO.
 01  WK-TOK-F20            PIC  9(11) VALUE ZERO.
 01  WK-TOK-F21            PIC  9(11) VALUE ZERO.
*2014/01/06↑
 01  COUNTER.
     03  LINE-CNT       PIC  9(03)   VALUE  ZERO.
     03  PAGE-CNT       PIC  9(03)   VALUE  ZERO.
     03  ERR-CD         PIC  9(02)   VALUE  ZERO.
 01  IDX.
     03  I              PIC  9(03)   VALUE  ZERO.
*出力範囲情報
 01  WK-HANI.
     03  WK-TANCD       PIC  X(02)   VALUE  ZERO.
     03  WK-NYUST       PIC  9(08)   VALUE  ZERO.
     03  WK-NYUED       PIC  9(08)   VALUE  ZERO.
     03  WK-TOKST       PIC  9(08)   VALUE  ZERO.
     03  WK-TOKED       PIC  9(08)   VALUE  ZERO.
     03  WK-DENST       PIC  9(09)   VALUE  ZERO.
     03  WK-DENED       PIC  9(09)   VALUE  ZERO.
     03  WK-SYONIN      PIC  X(01)   VALUE  ZERO.
***  2010/01/05 NAV 大野 BEGIN
     03  WK-SHIIRE      PIC  X(01)   VALUE  ZERO.
     03  WK-HENPIN      PIC  X(01)   VALUE  ZERO.
***  2010/01/05 NAV 大野 END
     03  WK-TIMES       PIC  9(06)   VALUE  ZERO.
     03  WK-TIMEE       PIC  9(06)   VALUE  ZERO.
*担当者エリア
 01  WK-TANTOUSYA       PIC  X(02)   VALUE  SPACE.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  HTANMS-ST         PIC  X(02).
*2014/01/06↓
 01  JYO-ST            PIC  X(02).
*2014/01/06↑
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
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  WK-32          PIC  X(25).
     03  WK-33          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-34          PIC  S9(12)V99   PACKED-DECIMAL.
     03  WK-35          PIC  S9(12)V99   PACKED-DECIMAL.
*2014/01/06↓
     03  WK-36          PIC   X(01).
*2014/01/06↑
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-01.
             07  NEW-TOR                    PIC  9(08).
             07  NEW-DEN                    PIC  X(09).
             07  NEW-KUB                    PIC  9(01).
             07  NEW-DKU                    PIC  9(02).
             07  NEW-TEN                    PIC  9(05).
             07  NEW-NOU                    PIC  9(08).
     03  OLD.
         05  OLD-01.
             07  OLD-TOR                    PIC  9(08).
             07  OLD-DEN                    PIC  X(09).
             07  OLD-KUB                    PIC  9(01).
             07  OLD-DKU                    PIC  9(02).
             07  OLD-TEN                    PIC  9(05).
             07  OLD-NOU                    PIC  9(08).
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF09           PIC  X(04)     VALUE     "F009".
*
***  ﾒｯｾｰｼﾞｴﾘｱ
 01  MSG-AREA.
     03  MSG-FIELD.
         05  MSG01                PIC  N(30)     VALUE
         NC"無効キーです。".
         05  MSG02                PIC  N(30)     VALUE
         NC"担当者マスタ未入力です。".
         05  MSG03                PIC  N(30)     VALUE
         NC"入力日を入力して下さい。".
         05  MSG04                PIC  N(30)     VALUE
         NC"開始が終了を超えています。".
         05  MSG05                PIC  N(30)     VALUE
         NC"未承認区分エラーです。".
         05  MSG06                PIC  N(30)     VALUE
         NC"日付論理エラーです。".
*2010/01/05)NAV 大野 BEGIN
         05  MSG07                PIC  N(30)     VALUE
         NC"仕入単価出力の入力エラーです。".
         05  MSG08                PIC  N(30)     VALUE
         NC"返品廃棄伝票のみの、入力エラーです。".
*2010/01/05)NAV 大野 END
     03  MSG-FIELD-R     REDEFINES    MSG-FIELD.
         05  MSG-TBL     OCCURS     8      PIC  N(30).
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"_取　消　_終　了　_再入力".
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  X(08)     VALUE  "SSY0103L".
         05  FILLER     PIC  X(20)     VALUE  SPACE.
         05  FILLER     PIC  N(16)     VALUE
                        NC"【　手書伝票　チェックリスト　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(20)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  ZZZ9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
*2008/07/29)NAV 後閑 BEGIN
 01  HEAD04.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"入力担当者".
         05  FILLER     PIC  N(01)     VALUE  NC"：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-041     PIC  X(02).
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  HD-042     PIC  N(10).
*2008/07/29)NAV 後閑 END
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"相".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(27)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"出場".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝場".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(18)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝区".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文_".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"注文日".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"納品日".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"分類".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(02)     VALUE  NC"商区".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝票".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"伝発".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"請区".
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"行".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"量販商品".
         05  FILLER     PIC  X(47)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"数量".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"単".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価単価".
         05  FILLER     PIC  X(09)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"原価金額".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価単価".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"売価金額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  N(03)     VALUE  NC"スト_".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  HD-BI      PIC  N(04)     VALUE  NC"備　考".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-1-5.
         05  FILLER     PIC  X(02).
         05  ME-03      PIC  9(01).
         05  FILLER     PIC  X(01).
         05  ME-04      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-05      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-06      PIC  X(09).
         05  FILLER     PIC  X(02).
         05  ME-07      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-08      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-09      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-10      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  9(02).
         05  FILLER     PIC  X(01).
         05  ME-12      PIC  N(04).
         05  FILLER     PIC  X(01).
         05  ME-13      PIC  X(07).
         05  FILLER     PIC  X(01).
         05  ME-14      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-15      PIC  9(08).
         05  FILLER     PIC  X(01).
         05  ME-16      PIC  X(04).
         05  FILLER     PIC  X(03).
         05  ME-17      PIC  X(02).
         05  FILLER     PIC  X(02).
         05  ME-18      PIC  X(02).
         05  FILLER     PIC  X(03).
         05  ME-19      PIC  9(01).
         05  FILLER     PIC  X(05).
     03  FILLER         CHARACTER TYPE MODE-2.
         05  ME-20      PIC  N(02).
*2008/07/29)NAV 後閑 BEGIN
 01  MEIS04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"（登".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  ME-41      PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"更".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  ME-42      PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"承".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  ME-43      PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"入力日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  ME-44      PIC  9999.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  ME-45      PIC  99.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  ME-46      PIC  99.
         05  FILLER     PIC  N(01)     VALUE  NC"）".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-47      PIC  N(02).
*2008/07/29)NAV 後閑 END
 01  MEIS02.
     03  FILLER.
         05  FILLER     PIC  X(01).
         05  ME-21      PIC  Z9.
         05  FILLER     PIC  X(01).
         05  ME-22      PIC  X(13).
         05  FILLER     PIC  X(01).
         05  ME-23      PIC  X(30).
         05  ME-24      PIC  --,---,--9.99.
         05  FILLER     PIC  X(02).
         05  ME-25      PIC  X(01).
         05  ME-26      PIC  --,---,--9.99.
         05  ME-27      PIC  ---,---,--9.
         05  ME-28      PIC  --,---,--9.99.
         05  ME-29      PIC  ---,---,--9.
         05  FILLER     PIC  X(02).
         05  ME-30      PIC  X(05).
         05  FILLER     PIC  X(02).
*2010/01/05)NAV 大野 BEGIN
*        備考と仕入単価を切替えるための再定義
         05  ME-31A.
             07  ME-312     PIC  -------9.99.
         05  ME-31          REDEFINES      ME-31A.
             07  ME-311     PIC  X(10).
*2010/01/05)NAV 大野 END
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  ME-32L     PIC  N(03)     VALUE  NC"（備考".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-32      PIC  X(25).
         05  ME-32R     PIC  N(01)     VALUE  NC"）".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"合　計".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  ME-33      PIC  ---,---,--9.99.
         05  FILLER     PIC  X(15)     VALUE  SPACE.
         05  ME-34      PIC  ----,---,--9.
         05  FILLER     PIC  X(12)     VALUE  SPACE.
         05  ME-35      PIC  ----,---,--9.
*2014/01/06↓
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"計上税区".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  ME-36      PIC  N(05)     VALUE  SPACE.
*2014/01/06↑
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE             PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "=".
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
     03  LINK-OUT-YMD8      PIC   9(08).
*----<< ﾀﾝﾄｳｼｬﾏｽﾀ ﾌﾞﾓﾝCD >>--*
 01  TAN-BMN                PIC   X(04)    VALUE  "2910".
*
 LINKAGE                SECTION.
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-NYURYOKU-DATE     PIC   9(08).
 01  PARA-NYURYOKU-TIMES    PIC   9(06).
 01  PARA-NYURYOKU-TIMEE    PIC   9(06).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-BUMON
                                         PARA-TANCD
                                         PARA-NYURYOKU-DATE
                                         PARA-NYURYOKU-TIMES
                                         PARA-NYURYOKU-TIMEE.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0103L DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
*担当者マスタクローズ表記追加
     CLOSE    SHTDENF   HTOKMS    HTENMS    HTANMS    DSPFILE.
*2014/01/06↓
     CLOSE    HJYOKEN.
*2014/01/06↑
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0103L SHTDENF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
*担当者マスタクローズ表記追加
     CLOSE    SHTDENF   HTOKMS    HTENMS    HTANMS    DSPFILE.
*2014/01/06↓
     CLOSE    HJYOKEN.
*2014/01/06↑
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0103L HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
*担当者マスタクローズ表記追加
     CLOSE    SHTDENF   HTOKMS    HTENMS    HTANMS    DSPFILE.
*2014/01/06↓
     CLOSE    HJYOKEN.
*2014/01/06↑
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0103L HTENMS ERROR " HTENMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
*担当者マスタクローズ表記追加
     CLOSE    SHTDENF   HTOKMS    HTENMS    HTANMS    DSPFILE.
*2014/01/06↓
     CLOSE    HJYOKEN.
*2014/01/06↑
     STOP     RUN.
*2008/07/29)NAV 後閑 BEGIN
*----<< 担当者マスタ >>--*
 HTANMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTANMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0103L HTANMS ERROR " HTANMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    HTENMS    HTANMS    DSPFILE.
*2014/01/06↓
     CLOSE    HJYOKEN.
*2014/01/06↑
     STOP     RUN.
*2008/07/29)NAV 後閑 END
*2014/01/06↓
*----<< 条件ファイル >>--*
 HJYOKEN-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY0103L HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    HTENMS    HTANMS    DSPFILE
              HJYOKEN.
     STOP     RUN.
*2014/01/06↑
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     DISPLAY NC"部門ＣＤ＝" PARA-BUMON UPON CONS.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY0103L START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     SHTDENF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
*2014/01/06↓
     OPEN     INPUT     HJYOKEN.
*2014/01/06↑
*2008/07/30)NAV 後閑 BEGIN
     OPEN     INPUT     HTANMS.
*2008/07/30)NAV 後閑 END
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
*    DISPLAY "PARA-BUMON          = " PARA-BUMON
*             UPON CONS.
*    DISPLAY "PARA-TANCD          = " PARA-TANCD
*             UPON CONS.
*    DISPLAY "PARA-NYURYOKU-DATE  = " PARA-NYURYOKU-DATE
*             UPON CONS.
*    DISPLAY "PARA-NYURYOKU-TIMES = " PARA-NYURYOKU-TIMES
*             UPON CONS.
*    DISPLAY "PARA-NYURYOKU-TIMEE = " PARA-NYURYOKU-TIMEE
*             UPON CONS.
     IF       PARA-NYURYOKU-TIMES  NUMERIC
     AND      PARA-NYURYOKU-TIMES  >  ZERO
**************手書伝票入力より起動
              MOVE PARA-TANCD             TO  WK-TANCD
              MOVE PARA-NYURYOKU-DATE     TO  WK-NYUST  WK-NYUED
              MOVE PARA-NYURYOKU-TIMES    TO  WK-TIMES
              MOVE PARA-NYURYOKU-TIMEE    TO  WK-TIMEE
              MOVE ZERO                   TO  WK-TOKST  WK-DENST
              MOVE 99999999               TO  WK-TOKED
              MOVE 999999999              TO  WK-DENED
              MOVE SPACE                  TO  WK-SYONIN
              MOVE SPACE                  TO  HENPIN
              MOVE SPACE                  TO  SHIIRE
              MOVE "1"                    TO  AUTO-FLG
              MOVE 10                     TO  GR-NO
     ELSE
**************範囲指定画面より出力条件入力
              MOVE " "                    TO  AUTO-FLG
              MOVE 0                      TO  GR-NO
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ｲﾝｻﾂ >>-*
     PERFORM  240-PRINT      UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    SHTDENF.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
*2008/07/29)NAV 後閑 BEGIN
     CLOSE    HTANMS.
*2008/07/29)NAV 後閑 END
*2014/01/06↓
     CLOSE    HJYOKEN.
*2014/01/06↑
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY0103L END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     DISPLAY NC"出力枚数" " = " PAGE-CNT  NC"枚" UPON CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   FSY01022.
*
     MOVE    "SSY0103L"      TO   PGID.
     MOVE    "FSY01022"      TO   FORM.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY01022"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "HEAD01"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              PERFORM   HEAD-CHK-SEC
              IF   ERR-CD  =  ZERO
                   MOVE    9      TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE     1          TO   ERR-CD
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 HEAD-CHK-SEC           SECTION.
*
     MOVE     "HEAD-CHK-SEC"      TO   S-NAME.
*
     MOVE      ZERO               TO   ERR-CD.
*
*入力担当者ﾁｪｯｸ
     IF   TANCD  =  SPACE
          MOVE  NC"全担当者"      TO   TANNM
     ELSE
          MOVE  PARA-BUMON        TO   TAN-F01
          MOVE  TANCD             TO   TAN-F02
          PERFORM 900-TAN-READ
          IF    HTANMS-INV-FLG = "INV"
                MOVE    2         TO   ERR-CD
                MOVE   "C"        TO   EDIT-CURSOR OF TANCD
                MOVE   "R"        TO   EDIT-OPTION OF TANCD
                GO                TO   220-INP-GRP02-EXIT
          ELSE
                MOVE   TAN-F03    TO   TANNM
                MOVE   " "        TO   EDIT-CURSOR OF TANCD
                MOVE   "M"        TO   EDIT-OPTION OF TANCD
          END-IF
     END-IF.
*入力日ﾁｪｯｸ（開始）
     IF   NYUST  NOT  NUMERIC
          IF   ERR-CD  =  ZERO
                MOVE    3         TO   ERR-CD
                MOVE   "C"        TO   EDIT-CURSOR OF NYUST
          END-IF
          MOVE   "R"              TO   EDIT-OPTION OF NYUST
          MOVE   ZERO             TO   NYUST
          GO                      TO   220-INP-GRP02-EXIT
     ELSE
          MOVE    "2"        TO        LINK-IN-KBN
          MOVE     NYUST     TO        LINK-IN-YMD8
          CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8
           IF      LINK-OUT-RET   =    ZERO
                   MOVE " "            TO   EDIT-CURSOR OF NYUST
                   MOVE "M"            TO   EDIT-OPTION OF NYUST
           ELSE
                   IF   ERR-CD  =  ZERO
                        MOVE    6      TO   ERR-CD
                        MOVE   "C"     TO   EDIT-CURSOR OF NYUST
                   END-IF
                   MOVE   "R"          TO   EDIT-OPTION OF NYUST
                   GO                  TO   220-INP-GRP02-EXIT
           END-IF
     END-IF.
*入力日ﾁｪｯｸ（終了）
     IF   NYUED  NOT  NUMERIC
          IF   ERR-CD  =  ZERO
                MOVE    3         TO   ERR-CD
                MOVE   "C"        TO   EDIT-CURSOR OF NYUED
          END-IF
          MOVE   "R"              TO   EDIT-OPTION OF NYUED
          MOVE   ZERO             TO   NYUED
          GO                      TO   220-INP-GRP02-EXIT
     ELSE
          MOVE    "2"        TO        LINK-IN-KBN
          MOVE     NYUED     TO        LINK-IN-YMD8
          CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8
           IF      LINK-OUT-RET   =    ZERO
                   MOVE " "            TO   EDIT-CURSOR OF NYUED
                   MOVE "M"            TO   EDIT-OPTION OF NYUED
           ELSE
                   IF   ERR-CD  =  ZERO
                        MOVE    6      TO   ERR-CD
                        MOVE   "C"     TO   EDIT-CURSOR OF NYUED
                   END-IF
                   MOVE   "R"          TO   EDIT-OPTION OF NYUED
                   GO                  TO   220-INP-GRP02-EXIT
           END-IF
     END-IF.
*入力日大小チェック
     IF   NYUST  >  NYUED
          IF   ERR-CD  =  ZERO
               MOVE    4      TO   ERR-CD
               MOVE   "C"     TO   EDIT-CURSOR OF NYUST
          END-IF
          MOVE   "R"          TO   EDIT-OPTION OF NYUST
          MOVE   "R"          TO   EDIT-OPTION OF NYUED
          GO                  TO   220-INP-GRP02-EXIT
     ELSE
          MOVE   " "          TO   EDIT-CURSOR OF NYUST
          MOVE   "M"          TO   EDIT-OPTION OF NYUST
          MOVE   "M"          TO   EDIT-OPTION OF NYUED
     END-IF.
*取引先CD範囲
     IF    TOKST  NOT  NUMERIC
           MOVE    ZERO                TO   TOKST
     END-IF.
*
     IF    TOKED  NOT  NUMERIC
           MOVE    99999999            TO   TOKED
     END-IF.
*
     IF   TOKST  >  TOKED
          IF   ERR-CD  =  ZERO
               MOVE    4      TO   ERR-CD
               MOVE   "C"     TO   EDIT-CURSOR OF TOKST
          END-IF
          MOVE   "R"          TO   EDIT-OPTION OF TOKST
          MOVE   "R"          TO   EDIT-OPTION OF TOKED
     ELSE
          MOVE   " "          TO   EDIT-CURSOR OF TOKST
          MOVE   "M"          TO   EDIT-OPTION OF TOKST
          MOVE   "M"          TO   EDIT-OPTION OF TOKED
     END-IF.
*伝票番号範囲
     IF    DENST  NOT  NUMERIC
           MOVE    ZERO                TO   DENST
     END-IF.
*
     IF    DENED  NOT  NUMERIC
           MOVE    999999999           TO   DENED
     END-IF.
*
     IF   DENST  >  DENED
          IF   ERR-CD  =  ZERO
               MOVE    4      TO   ERR-CD
               MOVE   "C"     TO   EDIT-CURSOR OF DENST
          END-IF
          MOVE   "R"          TO   EDIT-OPTION OF DENST
          MOVE   "R"          TO   EDIT-OPTION OF DENED
     ELSE
          MOVE   " "          TO   EDIT-CURSOR OF DENST
          MOVE   "M"          TO   EDIT-OPTION OF DENST
          MOVE   "M"          TO   EDIT-OPTION OF DENED
     END-IF.
*未承認区分チェック
     IF   SYONIN  =  SPACE  OR  "1"
          MOVE   " "          TO   EDIT-CURSOR OF SYONIN
          MOVE   "M"          TO   EDIT-OPTION OF SYONIN
     ELSE
          IF   ERR-CD  =  ZERO
               MOVE    5      TO   ERR-CD
               MOVE   "C"     TO   EDIT-CURSOR OF SYONIN
          END-IF
          MOVE   "R"          TO   EDIT-OPTION OF SYONIN
          MOVE   "R"          TO   EDIT-OPTION OF SYONIN
     END-IF.
***  2010/01/05 NAV 大野 BEGIN
*仕入単価出力の入力確認
     IF   SHIIRE  =  SPACE  OR  "1"
          MOVE   " "          TO   EDIT-CURSOR OF SHIIRE
          MOVE   "M"          TO   EDIT-OPTION OF SHIIRE
*         ヘッダの仕入単価と備考の表示切替
          IF   SHIIRE  =   "1"
               MOVE    NC"仕入単価"   TO   HD-BI
          ELSE
               MOVE    NC"備　考"     TO   HD-BI
          END-IF
     ELSE
          IF   ERR-CD  =  ZERO
               MOVE    7      TO   ERR-CD
               MOVE   "C"     TO   EDIT-CURSOR OF SHIIRE
          END-IF
          MOVE   "R"          TO   EDIT-OPTION OF SHIIRE
          MOVE   "R"          TO   EDIT-OPTION OF SHIIRE
     END-IF.
*返品廃棄伝票のみかの、入力確認
     IF   HENPIN  =  SPACE  OR  "1"
          MOVE   " "          TO   EDIT-CURSOR OF HENPIN
          MOVE   "M"          TO   EDIT-OPTION OF HENPIN
     ELSE
          IF   ERR-CD  =  ZERO
               MOVE    8      TO   ERR-CD
               MOVE   "C"     TO   EDIT-CURSOR OF HENPIN
          END-IF
          MOVE   "R"          TO   EDIT-OPTION OF HENPIN
          MOVE   "R"          TO   EDIT-OPTION OF HENPIN
     END-IF.
***  2010/01/05 NAV 大野 END
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KAKU"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF09
              MOVE      1         TO   GR-NO
         WHEN ENT
              MOVE      TANCD     TO   WK-TANCD
              MOVE      NYUST     TO   WK-NYUST
              MOVE      NYUED     TO   WK-NYUED
      DISPLAY "WK-NYUST = " WK-NYUST UPON CONS
      DISPLAY "WK-NYUED = " WK-NYUED UPON CONS
              MOVE      TOKST     TO   WK-TOKST
              MOVE      TOKED     TO   WK-TOKED
              MOVE      DENST     TO   WK-DENST
              MOVE      DENED     TO   WK-DENED
              MOVE      SYONIN    TO   WK-SYONIN
***  2010/01/05 NAV 大野 BEGIN
              MOVE      SHIIRE    TO   WK-SHIIRE
              MOVE      HENPIN    TO   WK-HENPIN
***  2010/01/05 NAV 大野 END
              MOVE      ZERO      TO   WK-TIMES
              MOVE      999999    TO   WK-TIMEE
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE     1          TO   ERR-CD
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 240-PRINT              SECTION.
     MOVE     "240-PRINT"    TO   S-NAME.
     OPEN     OUTPUT    PRTF.
     MOVE     99             TO   LINE-CNT.
     MOVE     0              TO   PAGE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE                   GOKEI-AREA.
*
     MOVE     SPACE              TO         DEN-REC.
     INITIALIZE                             DEN-REC.
     PERFORM  900-DEN-START-READ.
     PERFORM  241-LIST-PRINT
                        UNTIL     OLD  =    HIGH-VALUE.
     CLOSE    PRTF.
*
     IF  AUTO-FLG = SPACE
         MOVE     0              TO   GR-NO
     ELSE
         MOVE     99             TO   GR-NO
     END-IF.
*
 240-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｲﾝｻﾂ                                        *
*--------------------------------------------------------------*
 241-LIST-PRINT         SECTION.
     MOVE    "241-LIST-PRINT"     TO   S-NAME.
*合計行出力
     IF       OLD  NOT  =    LOW-VALUE
     AND      NEW  NOT  =    OLD
              PERFORM   2413-MEIS03-PRINT
     END-IF.
*担当者ＣＤﾁｪｯｸ
     IF       NEW  NOT  =    HIGH-VALUE
     AND      WK-TANTOUSYA  NOT =  DEN-F60
              PERFORM 2415-HEAD-PRINT
              MOVE  DEN-F60  TO    WK-TANTOUSYA
     END-IF.
*明細ヘッダ出力
     IF       NEW  NOT  =    HIGH-VALUE
     AND      NEW  NOT  =    OLD
              PERFORM   2411-MEIS01-PRINT
     END-IF.
*明細出力
     IF       NEW       NOT  =    HIGH-VALUE
     AND      DEN-F03   NOT  =    80
              PERFORM   2412-MEIS02-PRINT
     END-IF.
*
     MOVE     NEW            TO   OLD.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   2414-SYUKEI
              PERFORM   900-DEN-READ
     END-IF.
 241-LIST-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2411-MEIS01-PRINT      SECTION.
     MOVE    "2411-MEIS01-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    53
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS01.
     MOVE     DEN-F04        TO   ME-03.
     MOVE     DEN-F01        TO   ME-04.
     MOVE     DEN-F01        TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   ME-05.
     MOVE     DEN-F02        TO   ME-06.
     MOVE     DEN-F08        TO   ME-07.
     MOVE     DEN-F09        TO   ME-08.
     MOVE     DEN-F07        TO   ME-09.
     MOVE     DEN-F01        TO   TEN-F52.
     MOVE     DEN-F07        TO   TEN-F011.
     PERFORM  900-TEN-READ.
     MOVE     TEN-F03        TO   ME-10.
     MOVE     DEN-F051       TO   ME-11.
     MOVE     DEN-F052       TO   ME-12.
     MOVE     DEN-F44(9:7)   TO   ME-13.
     MOVE     DEN-F111       TO   ME-14.
     MOVE     DEN-F112       TO   ME-15.
     MOVE     DEN-F12        TO   ME-16.
     MOVE     DEN-F131       TO   ME-17.
     MOVE     DEN-F132       TO   ME-18.
     MOVE     DEN-F134       TO   ME-19.
     IF       DEN-F133  =    9
              MOVE      NC"請求"  TO   ME-20
     ELSE
              MOVE      SPACE     TO   ME-20
     END-IF.
*
*2008/07/29)NAV 後閑 BEGIN
     WRITE    PRT-REC        FROM MEIS01    AFTER     2.
*    WRITE    PRT-REC        FROM P-SPACE   AFTER     1.
*
*    MOVE     SPACE          TO   MEIS04.
     MOVE     DEN-F59        TO   ME-41.
     MOVE     DEN-F60        TO   ME-42.
     MOVE     DEN-F61        TO   ME-43.
     MOVE     DEN-F63(1:4)   TO   ME-44.
     MOVE     DEN-F63(5:2)   TO   ME-45.
     MOVE     DEN-F63(7:2)   TO   ME-46.
     IF       DEN-F53   =    0
              MOVE      NC"登録"  TO   ME-47
     ELSE
              MOVE      NC"修正"  TO   ME-47
     END-IF.
*
     WRITE    PRT-REC        FROM MEIS04    AFTER     1.
*2008/07/29)NAV 後閑 END
     ADD      3              TO   LINE-CNT.
 2411-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2412-MEIS02-PRINT      SECTION.
     MOVE    "2412-MEIS02-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    53
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE          TO   MEIS02.
     MOVE     DEN-F03        TO   ME-21.
     IF       DEN-F25   NOT  =    SPACE
              MOVE      DEN-F25   TO   ME-22
     ELSE
              MOVE      DEN-F141  TO   ME-22
     END-IF.
     MOVE     DEN-F142       TO   ME-23.
     MOVE     DEN-F15        TO   ME-24.
     MOVE     DEN-F16        TO   ME-25.
     MOVE     DEN-F172       TO   ME-26.
     MOVE     DEN-F181       TO   ME-27.
     MOVE     DEN-F173       TO   ME-28.
     MOVE     DEN-F182       TO   ME-29.
     MOVE     DEN-F21        TO   ME-30.
*2010/01/05)NAV 大野 BEGIN
*    明細部の仕入れ単価と備考の切替
     IF       SHIIRE    NOT = SPACE
              MOVE      DEN-F171  TO   ME-312
     ELSE
              MOVE      DEN-F22   TO   ME-311
     END-IF.
*2010/01/05)NAV 大野 END
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS02    AFTER     1.
     ADD      1              TO   LINE-CNT.
 2412-MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ                                    *
*--------------------------------------------------------------*
 2413-MEIS03-PRINT      SECTION.
     MOVE    "2413-MEIS03-PRINT"  TO   S-NAME.
     IF       LINE-CNT  >    53
              PERFORM   2415-HEAD-PRINT
     END-IF.
*
     IF       WK-32     NOT  =    SPACE
              MOVE      WK-32          TO   ME-32
              MOVE      NC"備考（"     TO   ME-32L
              MOVE      NC"）"         TO   ME-32R
     ELSE
              MOVE      SPACE          TO   ME-32
              MOVE      SPACE          TO   ME-32L
              MOVE      SPACE          TO   ME-32R
     END-IF.
     MOVE     WK-33          TO   ME-33.
     MOVE     WK-34          TO   ME-34.
     MOVE     WK-35          TO   ME-35.
*2014/01/06↓
*    <計上税区分>
*##2014/01/23 ↓
     IF   WK-36  NOT =  SPACE
          MOVE     50                     TO  JYO-F01
          MOVE     WK-36                  TO  JYO-F02
          PERFORM  HJYOKEN-READ-SUB
          IF       JYO-INVALID-FLG   =    1
                   MOVE  NC"＊＊＊＊＊"   TO  ME-36
          ELSE
                   MOVE  JYO-F03          TO  ME-36
          END-IF
     ELSE
              MOVE  SPACE            TO  ME-36
     END-IF.
*##2014/01/23 ↑
*2014/01/06↑
*
     IF       LINE-CNT  <    7
              WRITE     PRT-REC   FROM P-SPACE   AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     WRITE    PRT-REC        FROM P-LINE    AFTER     1.
     ADD      3              TO   LINE-CNT.
     INITIALIZE              GOKEI-AREA.
 2413-MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 2414-SYUKEI            SECTION.
     MOVE    "2414-SYUKEI"   TO   S-NAME.
     IF       DEN-F03   =    80
              MOVE      DEN-F142       TO   WK-32
     END-IF.
     ADD      DEN-F15        TO   WK-33.
     ADD      DEN-F181       TO   WK-34.
     ADD      DEN-F182       TO   WK-35.
*2014/01/06↓
     MOVE     DEN-F34        TO   WK-36.
*2014/01/06↑
 2414-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 2415-HEAD-PRINT        SECTION.
     MOVE    "2415-HEAD-PRINT"    TO   S-NAME.
     IF       PAGE-CNT  >    0
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     SYS-YYW             TO   HD-011.
     MOVE     SYS-MMW             TO   HD-012.
     MOVE     SYS-DDW             TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-02.
*2008/07/29)NAV 後閑 BEGIN
     MOVE     PARA-BUMON          TO   TAN-F01.
     MOVE     DEN-F60             TO   TAN-F02.
     PERFORM  900-TAN-READ.
     MOVE     DEN-F60             TO   HD-041.
     MOVE     TAN-F03             TO   HD-042.
*2008/07/29)NAV 後閑 END
*
*2008/07/29)NAV 後閑 BEGIN
*    WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
*    WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
*    WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      P-LINE1   AFTER     1.
*2008/07/29)NAV 後閑 END
     MOVE     6              TO   LINE-CNT.
 2415-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
     IF       ERR-CD  NOT  =  ZERO
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
     MOVE     SPACE          TO   DSP-PRO.
*
     IF       ERR-CD   =   ZERO
              MOVE   SPACE                 TO   MSG
     ELSE
              MOVE   MSG-TBL ( ERR-CD )    TO   MSG
              MOVE   ZERO                  TO   ERR-CD
     END-IF.
*
     MOVE     "SCREEN"       TO   DSP-GRP.
     WRITE    FSY01022.
*
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
     MOVE     "900-DEN-START-READ"     TO   S-NAME.
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     WK-TANCD       TO   DEN-F60.
     MOVE     WK-NYUST       TO   DEN-F63.
*****MOVE     ZERO           TO   DEN-F63.
     START    SHTDENF   KEY  >=   DEN-F63   DEN-F60    DEN-F01
                                  DEN-F02   DEN-F04    DEN-F051
***2011.10.06(DEN-F07,DEN-F112)
                                  DEN-F07   DEN-F112   DEN-F03
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-READ
     END-IF.
 900-DEN-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
     READ     SHTDENF   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-READ.
*    DISPLAY "DEN-F01  = " DEN-F01  UPON CONS.
*    DISPLAY "DEN-F02  = " DEN-F02  UPON CONS.
*    DISPLAY "DEN-F63  = " DEN-F63  UPON CONS.
*    DISPLAY "DEN-F64  = " DEN-F64  UPON CONS.
*    DISPLAY "DEN-F67  = " DEN-F67  UPON CONS.
 900-DEN-010.
*入力日付
     IF       WK-NYUED  <  DEN-F63
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-IF.
 900-DEN-020.
*担当者CD
     IF       WK-TANCD  NOT =  SPACE
              IF   WK-TANCD  NOT =  DEN-F60
                   GO        TO   900-DEN-READ
              END-IF
     END-IF.
 900-DEN-030.
*取引先CDﾁｪｯｸ
     IF       DEN-F01   >=   WK-TOKST
     AND      DEN-F01   <=   WK-TOKED
              CONTINUE
     ELSE
              GO        TO   900-DEN-READ
     END-IF.
 900-DEN-040.
*伝票番号範囲ﾁｪｯｸ
     IF       DEN-F02   >=   WK-DENST
     AND      DEN-F02   <=   WK-DENED
              CONTINUE
     ELSE
              GO        TO   900-DEN-READ
     END-IF.
 900-DEN-050.
*登録／更新時間ﾁｪｯｸ
*    DISPLAY "DEN-F67  = " DEN-F67  UPON CONS.
*    DISPLAY "WK-TIMES = " WK-TIMES UPON CONS.
*    DISPLAY "WK-TIMEE = " WK-TIMEE UPON CONS.
     IF       DEN-F67   >=   WK-TIMES
     AND      DEN-F67   <=   WK-TIMEE
              CONTINUE
     ELSE
              GO        TO   900-DEN-READ
     END-IF.
 900-DEN-060.
*未承認ﾁｪｸ
     IF       WK-SYONIN  NOT =  SPACE
              IF   DEN-F64  =  ZERO
                   CONTINUE
              ELSE
                   GO        TO   900-DEN-READ
              END-IF
     END-IF.
 900-DEN-070.
*オンライン区分チェック
     IF       DEN-F274  =  0
              CONTINUE
     ELSE
              GO        TO   900-DEN-READ
     END-IF.
 900-DEN-080.
*売上区分チェック
     IF       DEN-F277  NOT =  9
              CONTINUE
     ELSE
              GO        TO   900-DEN-READ
     END-IF.
*
     IF       NEW       NOT  =    HIGH-VALUE
              MOVE      DEN-F01        TO   NEW-TOR
              MOVE      DEN-F02        TO   NEW-DEN
              MOVE      DEN-F04        TO   NEW-KUB
              MOVE      DEN-F051       TO   NEW-DKU
              MOVE      DEN-F07        TO   NEW-TEN
              MOVE      DEN-F112       TO   NEW-NOU
     END-IF.
*2010/01/05)NAV 大野 BEGIN
*伝区コードのチェック
****返品廃棄伝票のみを出力するかの判定
     IF       HENPIN    NOT =  SPACE
              IF        DEN-F051   =  47
                        CONTINUE
              ELSE
                        GO        TO   900-DEN-READ
              END-IF
     END-IF.

*2010/01/05)NAV 大野 END
*
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     "900-TEN-READ"      TO   S-NAME.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
              MOVE     "INV"           TO   HTENMS-INV-FLG
              NOT  INVALID
              MOVE      SPACE          TO   HTENMS-INV-FLG
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    担当者マスタ　READ                           *
*--------------------------------------------------------------*
 900-TAN-READ           SECTION.
     MOVE     "900-TAN-READ"      TO   S-NAME.
     READ     HTANMS    INVALID
              MOVE      SPACE          TO   TAN-F03
              MOVE     "INV"           TO   HTANMS-INV-FLG
              NOT  INVALID
              MOVE      SPACE          TO   HTANMS-INV-FLG
     END-READ.
 900-TAN-READ-EXIT.
     EXIT.
*2014/01/06↓
****************************************************************
*      3.0        条件ファイル索引                             *
****************************************************************
 HJYOKEN-READ-SUB       SECTION.
     MOVE     0                  TO   JYO-INVALID-FLG.
     READ     HJYOKEN
        INVALID
              MOVE      1        TO   JYO-INVALID-FLG
     END-READ.
 HJYOKEN-READ-END.
     EXIT.
*2014/01/06↑
*-----------------<< PROGRAM END >>----------------------------*

```
