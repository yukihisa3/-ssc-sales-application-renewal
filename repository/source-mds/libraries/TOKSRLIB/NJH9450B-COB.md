# NJH9450B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NJH9450B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　　　　　　　*
*    サブシステム　　　　：　ヨドバシ　ＥＤＩ　　　　　　　    *
*    モジュール名　　　　：　ヨドバシ受注データ変換（発注）　  *
*    作成日／作成者　　　：　2021/07/28 INOUE NJH9420B
*    処理概要　　　　　　：　取込ワーク（発注）より、
*                            伝票更新処理（現行）に引き渡す。
*                            また、実績管理用に、
*                            基本情報ファイルを作成する。
*    更新日／更新者　　　：　2022/02/14 TAKAHASHI NJH9420B
*    更新内容　　　　　　：  付番ＦＬＧセット方法変更
*    　　　　　　　　　　：  ９→０へ変更　
*    作成日／作成者　　　：　2023/01/25 INOUE NJH9450B
*    更新内容　　　　　　：  納品日算出/セット
*    　　　　　　　　　　　  納期変更/出荷確定ファイル追加
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            NJH9450B.
*                 流用：NJH9420B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/01/25.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ヨドバシ取込ワーク（発注）
     SELECT   YODORDL1  ASSIGN    TO        DA-01-VI-YODORDL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       ORD-F03
                                            ORD-F02
                                            ORD-F07
                        FILE      STATUS    ORD-STATUS.
*ヨドバシ取込ワーク（発注）（件数・枚数カウント用）
     SELECT   YODORDL2  ASSIGN    TO        DA-01-VI-YODORDL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       OR2-F15
                                            OR2-F16
                                            OR2-F17
                                            OR2-F18
                                            OR2-F19
                                            OR2-F20
                        FILE      STATUS    OR2-STATUS.
*変換伝票データ(当日売上伝票ファイル)
     SELECT   JHSHENL1  ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEN-F46
                                            HEN-F47
                                            HEN-F01
                                            HEN-F02
                                            HEN-F03
                        FILE  STATUS   IS   HEN-STATUS.
*ヨドバシ基本情報ファイル
     SELECT   YODJOHL1  ASSIGN    TO        DA-01-VI-YODJOHL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       JOH-F17
                                            JOH-F03
                                            JOH-F02
                                            JOH-F07
                        FILE      STATUS    JOH-STATUS.
*当日スケジュールマスタ
     SELECT   JSMDAYL1  ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*受信件数マスタ
     SELECT   JSMKENL1  ASSIGN    TO        DA-01-VI-JSMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   MEIMS1   ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011  MEI-F0121
                                            MEI-F0122 MEI-F0123
                        FILE      STATUS    MEI-STATUS.
*20230125↓
*日次条件マスタ
     SELECT   JHMNITL1  ASSIGN    TO        DA-01-VI-JHMNITL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       NIT-F01
                        FILE  STATUS   IS   NIT-STATUS.
*条件ファイル
     SELECT   JYOKEN1   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE  STATUS   IS   JYO-STATUS.
*ヨドバシ納期変更／出荷確定ファイル
     SELECT   YODNKKL1  ASSIGN    TO        DA-01-VI-YODNKKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       NKK-F01
                                            NKK-F02
                                            NKK-F03
                                            NKK-F04
                                            NKK-F05
                                            NKK-F06
                                            NKK-F07
                                            NKK-F08
                                            NKK-F09
                                            NKK-F10
                                            NKK-F11
                        FILE  STATUS   IS   NKK-STATUS.
*20230125↑
*
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
******************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ヨドバシ取込ワーク（発注）
******************************************************************
 FD  YODORDL1            LABEL RECORD   IS   STANDARD.
     COPY     YODORDPF   OF        XFDLIB
              JOINING    ORD       PREFIX.
******************************************************************
*    ヨドバシ取込ワーク（発注）
******************************************************************
 FD  YODORDL2            LABEL RECORD   IS   STANDARD.
     COPY     YODORDL2   OF        XFDLIB
              JOINING    OR2       PREFIX.
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     NFSHIRED  OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
******************************************************************
*    ヨドバシ基本情報ファイル
******************************************************************
 FD  YODJOHL1
                        LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL1  OF        XFDLIB
              JOINING   JOH  AS   PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    受信件数ファイル
******************************************************************
 FD  JSMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1            LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1   OF        XFDLIB
              JOINING  MEI       PREFIX.
*20230125↓
******************************************************************
*    日次条件マスタ
******************************************************************
 FD  JHMNITL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMNITL1  OF        XFDLIB
              JOINING   NIT       PREFIX.
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKEN1            LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
******************************************************************
*    ヨドバシ納期変更／出荷確定ファイル
******************************************************************
 FD  YODNKKL1           LABEL RECORD   IS   STANDARD.
     COPY     YODNKKL1  OF        XFDLIB
              JOINING   NKK       PREFIX.
*20230125↑
*
******************************************************************
*    ＶＬＤ５００
******************************************************************
 FD  VLD500.
 01  VLD-REC.
     03  VLD-F01           PIC  X(02).
     03  VLD-F02           PIC  9(03).
     03  VLD-F03           PIC  X(02).
     03  VLD-F04           PIC  X(08).
     03  VLD-F05           PIC  9(06).
     03  VLD-F06           PIC  9(01).
     03  VLD-F07           PIC  X(02).
     03  VLD-F08           PIC  9(02).
     03  VLD-F09           PIC  9(02).
     03  VLD-F10           PIC  9(04).
     03  VLD-F11           PIC  9(08).
     03  VLD-F12           PIC  9(04).
     03  VLD-F13           PIC  9(08).
     03  FILLER            PIC  X(48).
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU-D             PIC  9(08)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(08)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-RUTO                 PIC  X(02)     VALUE  SPACE.
 01  WK-SAIBAN               PIC  9(11)     VALUE  ZERO.
 01  SAKUBAF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SHOTBL1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMNITL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  JYOKEN1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-JOH-F21              PIC  9(05)V9(01)  VALUE  ZERO.
 01  WK-JOH-F21-1            PIC  9(05)     VALUE  ZERO.
 01  FURISOK                 PIC  X(02)     VALUE  SPACE.
 01  OR2-END                 PIC  X(03)     VALUE  SPACE.
 01  WK-YOUBI.
     03  W-YOUBI             PIC  9(01)     VALUE  ZERO.
     03  W-SPACE             PIC  X(07)     VALUE  SPACE.
 01  WK-NPULS                PIC  9(02)     VALUE  ZERO.
 01  WK-NPULS-ENCHI          PIC  9(02)     VALUE  ZERO.
*
*店舗コード
 01  WK-TEN-F011.
     03  WK-TEN-F011-1      PIC   9(01)  VALUE  ZERO.
     03  WK-TEN-F011-2      PIC   9(04)  VALUE  ZERO.
 01  WK-TEN-F011-R REDEFINES WK-TEN-F011.
     03  WK-TEN-F011-RR     PIC   9(05).
*店舗コード
 01  WK-JYO-F02.
     03  WK-JYO-F02-1       PIC   9(04)  VALUE  ZERO.
     03  WK-JYO-F02-2       PIC   X(04)  VALUE  SPACE.
*
*伝票ブレイク（ヨドバシ発注書番号系）　非使用
 01  BRK-FA04                PIC  X(08)     VALUE  ZERO.
 01  BRK-FA03                PIC  X(10)     VALUE  SPACE.
 01  BRK-FA09                PIC  9(03)     VALUE  ZERO.
*
*伝票ブレイク（件数・枚数カウント用）
 01  BRK-F15                PIC  9(08)     VALUE  ZERO.
 01  BRK-F16                PIC  9(04)     VALUE  ZERO.
 01  BRK-F17                PIC  9(08)     VALUE  ZERO.
 01  BRK-F18                PIC  X(02)     VALUE  SPACE.
 01  BRK-F19                PIC  X(09)     VALUE  ZERO.
*
*基幹伝票行カウント
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
*
*伝票番号取得ＳＵＢ用パラメタ
 01  LINK-AREA-SUB1.
     03  LI-TRCD             PIC  9(08).
     03  LO-ERR              PIC  9(01).
     03  LO-DENNO            PIC  9(09).
     03  LO-NEXT             PIC  9(09).
 01  WRK-DEN.
     03  OUT-DENNO           PIC  9(09)     VALUE  ZERO.
*
*ルート変換　非使用
 01  HEN-RUT.
     03  HEN-RUT-1           PIC  X(01)     VALUE  "0".
     03  HEN-RUT-2           PIC  X(01)     VALUE  SPACE.
*伝票NO編集  非使用
 01  HEN-DEN.
     03  HEN-ORD-TEN         PIC  9(02)     VALUE  ZERO.
     03  HEN-ORD-DEN         PIC  9(07)     VALUE  ZERO.
*
 01  WK-HEN-A24.
     03  WK-ORD-A83          PIC  9(03).
     03  FILLER              PIC  X(01)     VALUE  SPACE.
     03  WK-ORD-A87          PIC  X(15).
*
 01  WK-HEN-A28.
     03  FILLER              PIC  X(07)     VALUE  "ｺｳｺｸNO:".
     03  WK-ORD-A81          PIC  9(08).
*
 01  WK-ORD-A56              PIC  9(05)V9(01).
 01  WK-ORD-A56R             REDEFINES      WK-ORD-A56.
     03  WK-HEN-A41          PIC  9(05).
     03  WK-HEN-A42          PIC  9(01).
*
 01  WK-ORD-A63              PIC  9(07)V9(02).
 01  WK-ORD-A63R             REDEFINES      WK-ORD-A63.
     03  WK-HEN-A43          PIC  9(07).
     03  WK-HEN-A44          PIC  9(02).
*伝票番号
*01  WK-DENNO                PIC  9(07).
*
*伝票ヘッダ退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(02).
     03  WK-DEPB03          PIC  X(02).
     03  WK-DEPB04          PIC  9(07).
     03  WK-DEPB05.
       05  WK-DEPB051       PIC  9(02).
       05  WK-DEPB052       PIC  9(03).
     03  WK-DEPB06          PIC  X(04).
     03  WK-DEPB07          PIC  X(02).
     03  WK-DEPB08          PIC  9(02).
     03  WK-DEPB09          PIC  9(02).
     03  WK-DEPB10          PIC  9(06).
     03  WK-DEPB11          PIC  9(06).
     03  WK-DEPB12          PIC  9(06).
     03  WK-DEPB13          PIC  X(02).
     03  WK-DEPB14          PIC  X(15).
     03  WK-DEPB15          PIC  X(15).
     03  WK-DEPB16          PIC  X(15).
     03  WK-DEPB17          PIC  X(12).
     03  WK-DEPB18          PIC  9(01).
     03  WK-DEPB181         PIC  X(01).
     03  WK-DEPB19          PIC  X(21).
     03  WK-DEPB20          PIC  9(01).
*
*    伝票ヘッダ・オプション退避ワーク
 01  WK-DEPC-REC.
     03  WK-DEPC01          PIC  X(01).
     03  WK-DEPC02          PIC  9(02).
     03  WK-DEPC03          PIC  X(07).
     03  WK-DEPC04          PIC  X(26).
     03  WK-DEPC05          PIC  X(14).
     03  WK-DEPC06          PIC  X(07).
     03  WK-DEPC07          PIC  X(05).
     03  WK-DEPC08          PIC  X(15).
     03  WK-DEPC09          PIC  X(07).
     03  WK-DEPC10          PIC  X(16).
     03  WK-DEPC11          PIC  X(20).
     03  WK-DEPC12          PIC  X(07).
     03  WK-DEPC13          PIC  9(01).
*
*    伝票明細行退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X.
     03  WK-DEPD02          PIC  9(02).
     03  WK-DEPD03          PIC  9(02).
     03  WK-DEPD04          PIC  X(13).
     03  WK-DEPD05          PIC  X(06).
     03  WK-DEPD06          PIC  X(01).
     03  WK-DEPD07          PIC  X(03).
     03  WK-DEPD08          PIC  9(05).
     03  WK-DEPD09          PIC  X(01).
     03  WK-DEPD10          PIC  9(07).
     03  WK-DEPD11          PIC  X(02).
     03  WK-DEPD12          PIC  9(07).
     03  WK-DEPD13          PIC  9(10).
     03  WK-DEPD14          PIC  9(10).
     03  WK-DEPD15          PIC  X(09).
     03  WK-DEPD16          PIC  X(25).
     03  WK-DEPD17          PIC  X(08).
     03  WK-DEPD18          PIC  X(06).
     03  WK-DEPD19          PIC  X(05).
     03  WK-DEPD20          PIC  X(04).
     03  WK-DEPD21          PIC  9(01).
*
*    伝票明細行オプション退避ワーク
 01  WK-DEPE-REC.
     03  WK-DEPE01          PIC  X.
     03  WK-DEPE02          PIC  9(02).
     03  WK-DEPE03          PIC  X(25).
     03  WK-DEPE04          PIC  X.
     03  WK-DEPE05          PIC  X.
     03  WK-DEPE06          PIC  9(02).
     03  WK-DEPE07          PIC  X(13).
     03  WK-DEPE08          PIC  X(20).
     03  WK-DEPE09          PIC  X(20).
     03  WK-DEPE10          PIC  9(07).
     03  WK-DEPE11          PIC  9(05).
     03  WK-DEPE12          PIC  X(30).
     03  WK-DEPE13          PIC  X(01).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  ORD-STATUS        PIC  X(02).
     03  OR2-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  NIT-STATUS        PIC  X(02).
     03  NKK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH9450B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9450B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9450B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6.
       05  LINK-IN-YMD61    PIC   9(04).
       05  LINK-IN-YMD62    PIC   9(02).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC   9(08)  VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*20220125↓
 01      PARA-TANCD         PIC   X(02).
 01      PARA-OUT-JKDATE    PIC   9(08).
 01      PARA-OUT-JKTIME    PIC   9(06).
*20220125↑
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA
                                       PARA-TANCD
                                       PARA-OUT-JKDATE
                                       PARA-OUT-JKTIME.
*
 DECLARATIVES.
 FILEERR-SEC0           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODORDL1.
     MOVE      "YODORDL1"   TO   AB-FILE.
     MOVE      ORD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODORDL2.
     MOVE      "YODORDL2"   TO   AB-FILE.
     MOVE      OR2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSHENL1.
     MOVE      "JHSHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODJOHL1.
     MOVE      "YODJOHL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMDAYL1.
     MOVE      "JSMDAYL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMKENL1.
     MOVE      "JSMKENL1"   TO   AB-FILE.
     MOVE      KEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1 "    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   VLD500.
     MOVE      "VLD500  "   TO   AB-FILE.
     MOVE      VLD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*20230125↓
 FILEERR-SEC11          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMNITL1.
     MOVE      "JHMNITL1"   TO   AB-FILE.
     MOVE      NIT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKEN1.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC13          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODNKKL1.
     MOVE      "YODNKKL1"   TO   AB-FILE.
     MOVE      NKK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*20220125↑
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       YODORDL1.
     OPEN     INPUT     SHOTBL1   MEIMS1
                        TENMS1    TOKMS2.
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JSMKENL1  JSMDAYL1  YODJOHL1.
     OPEN     OUTPUT    VLD500.
*20230125↓
     OPEN     INPUT     JHMNITL1  JYOKEN1.
     OPEN     I-O       YODNKKL1.
*20230125↑
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPC-REC.
     INITIALIZE                   WK-DEPC-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
     MOVE     SPACE     TO        WK-DEPE-REC.
     INITIALIZE                   WK-DEPE-REC.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*20230126↓
     MOVE      SYS-DATEW        TO   PARA-OUT-JKDATE.
*20230126↑
*
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*20230126↓
     MOVE      WK-TIME(1:6)     TO   PARA-OUT-JKTIME.
*20230126↑
*
 INIT-01.
*ﾖﾄﾞﾊﾞｼ取込ワーク（発注） READ
     READ     YODORDL1
              AT END
                 MOVE      9         TO  END-FG
                 GO                  TO  INIT-EXIT
              NOT AT END
                 ADD       1         TO  RD-CNT
     END-READ.
 INIT-02.
*変換済FLG＝""     のレコードのみ対象
     IF    ORD-F23     =  " "
*-         MOVE      ORD-F03        TO  BRK-FA04
*-         MOVE      ORD-F02        TO  BRK-FA03
           MOVE      0               TO  CNT-GYO
     ELSE
           GO                        TO  INIT-01
     END-IF.
*
*
*基幹伝票ＮＯ採番
*#１伝票１行
*##  PERFORM   DPNO-SET-SEC.
*##  ADD       1           TO   CNT-MAISU.
*#
*
 INIT-03.
*日次条件マスタ検索（曜日取得）
     MOVE      SYS-DATEW   TO   NIT-F01.
     READ      JHMNITL1
       INVALID MOVE  4010  TO   PROGRAM-STATUS
               DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃"
                                                  UPON CONS
               DISPLAY NC"＃　日次条件マスタなし！　＃"
                                                  UPON CONS
               DISPLAY NC"＃　　ＫＥＹ＝　"   SYS-DATEW
                                                  UPON CONS
               DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃"
                                                  UPON CONS
               MOVE  9     TO   END-FG
               GO          TO   INIT-EXIT
       NOT INVALID
               MOVE  NIT-F09    TO   W-YOUBI
*T
       DISPLAY NC"システム日付＝" SYS-DATEW UPON CONS
       DISPLAY NC"曜日　　　　＝" W-YOUBI   UPON CONS
*T
     END-READ.
*条件ファイル検索（納品日加算数）
*　　　　　　　　（遠地納品日加算数）
     MOVE      30          TO   JYO-F01.
     MOVE      WK-YOUBI    TO   JYO-F02.
     READ      JYOKEN1
       INVALID MOVE  4010  TO   PROGRAM-STATUS
               DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃"
                                                  UPON CONS
               DISPLAY NC"＃　条件ファイルなし！　　＃"
                                                  UPON CONS
               DISPLAY NC"＃　　ＫＥＹ１＝"   "30"
                                                  UPON CONS
               DISPLAY NC"＃　　ＫＥＹ２＝"    W-YOUBI
                                                  UPON CONS
               DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃"
                                                  UPON CONS
               MOVE  9     TO   END-FG
               GO          TO   INIT-EXIT
       NOT INVALID
               MOVE  JYO-F04    TO   WK-NPULS
               MOVE  JYO-F05    TO   WK-NPULS-ENCHI
*T
****         DISPLAY NC"近地＋数　　＝" WK-NPULS  UPON CONS
****         DISPLAY NC"遠地＋数　　＝" WK-NPULS-ENCHI UPON CONS
*T
     END-READ.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*                  伝票番号取得
******************************************************************
 DPNO-SET-SEC                 SECTION.
     MOVE     "DPNO-SET-SEC"       TO   S-NAME.
*
     INITIALIZE                        LINK-AREA-SUB1.
*
 DPNO-010.
     MOVE      ORD-F17       TO        LI-TRCD.
     CALL     "SKYSBCK1"     USING     LINK-AREA-SUB1.
 DPNO-020.
*↓TEST
*    DISPLAY   "LO-ERR ="  LO-ERR    UPON CONS.
*    DISPLAY   "LO-DENO="  LO-DENNO  UPON CONS.
*    DISPLAY   "LO-NEXT="  LO-NEXT   UPON CONS.
*↑TEST
     IF       LO-ERR  =  0
              MOVE      LO-NEXT     TO        OUT-DENNO
              MOVE      SPACE       TO        TOKMS2-INV-FLG
     ELSE
              DISPLAY   NC"伝票_採番エラー"  UPON CONS
              DISPLAY   LI-TRCD               UPON CONS
              MOVE      "INV"       TO        TOKMS2-INV-FLG
              MOVE      4001        TO        PROGRAM-STATUS
              STOP  RUN
     END-IF.
*
 DPNO-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO    S-NAME.
*
 MAIN-01.
*変換済FLG＝""     のレコードのみ対象
     IF    ORD-F23     =  " "
           CONTINUE
     ELSE
           GO                    TO    MAIN-99
     END-IF.
*
 MAIN-02.
*基幹伝票ＮＯ採番
*#１伝票１行
     PERFORM   DPNO-SET-SEC.
     ADD       1               TO  CNT-MAISU.
     MOVE      1               TO  CNT-GYO.
*-   MOVE      ORD-F03         TO  BRK-FA04.
*-   MOVE      ORD-F02         TO  BRK-FA03.
*
 MAIN-03.
* 得意先マスタ検索
     MOVE    SPACE         TO    TOK-REC.
     INITIALIZE                  TOK-REC.
     MOVE    ORD-F17       TO    TOK-F01.
     READ    TOKMS2
             INVALID
                 MOVE  SPACE     TO    TOK-REC
                 INITIALIZE            TOK-REC
                 MOVE  "INV"     TO    TOKMS2-INV-FLG
     END-READ.
*
* 店舗マスタ検索
     MOVE    SPACE           TO    TEN-REC.
     INITIALIZE                    TEN-REC.
     MOVE    ORD-F17         TO    TEN-F52.
     MOVE    ORD-F06(1:4)    TO    WK-TEN-F011-2.
     MOVE    WK-TEN-F011-RR  TO    TEN-F011.
     READ    TENMS1
             INVALID
                 MOVE  SPACE     TO    TEN-REC
                 INITIALIZE            TEN-REC
                 MOVE  "INV"     TO    TENMS1-INV-FLG
     END-READ.
*
* 商品変換テーブル検索
     MOVE        SPACE     TO    TBL-REC.
     INITIALIZE                  TBL-REC.
     MOVE        ORD-F17   TO    TBL-F01.
     MOVE        ORD-F08   TO    TBL-F02.
     READ    SHOTBL1
             INVALID
                 MOVE  SPACE     TO    TBL-REC
                 INITIALIZE            TBL-REC
                 MOVE  "INV"     TO    SHOTBL1-INV-FLG
     END-READ
     MOVE    TBL-F04             TO    WK-RUTO.
*
* 商品名称マスタ検索
     MOVE        SPACE     TO    MEI-REC.
     INITIALIZE                  MEI-REC.
     IF          SHOTBL1-INV-FLG =     SPACE
                 MOVE  TBL-F031  TO    MEI-F011
                 MOVE  TBL-F0321 TO    MEI-F0121
                 MOVE  TBL-F0322 TO    MEI-F0122
                 MOVE  TBL-F0323 TO    MEI-F0123
                 PERFORM   MEIMS1-READ-SEC
                 IF    MEIMS1-INV-FLG  = "INV"
                       MOVE   SPACE    TO    MEI-REC
                       INITIALIZE            MEI-REC
                 END-IF
     END-IF.
*20230125↓
* 条件ファイル検索（遠地店舗特定）
     MOVE        SPACE     TO    JYO-REC.
     INITIALIZE                  JYO-REC.
*    IF          JYOKEN1-INV-FLG    =   SPACE
*                MOVE    31               TO  JYO-F01
*                MOVE    ORD-F06(1:4)     TO  WK-JYO-F02-1
*                MOVE    WK-JYO-F02       TO  JYO-F02
*T
*                DISPLAY NC"出荷先　　　＝"   JYO-F02 UPON CONS
*T
*                PERFORM JYOKEN1-READ-SEC
*                IF      JYOKEN1-INV-FLG  =  "INV"
*T
*                DISPLAY NC"　近地　　　＝"           UPON CONS
*T
*                        MOVE   SPACE     TO  JYO-REC
*                        INITIALIZE           JYO-REC
*                END-IF
*    END-IF.
     MOVE    31               TO  JYO-F01.
     MOVE    ORD-F06(1:4)     TO  WK-JYO-F02-1.
     MOVE    WK-JYO-F02       TO  JYO-F02.
*T
***  DISPLAY NC"出荷先　　　＝"   JYO-F02 UPON CONS.
*T
     PERFORM JYOKEN1-READ-SEC.
     IF      JYOKEN1-INV-FLG  =  "INV"
*T
**         DISPLAY NC"　近地　　　＝"           UPON CONS
*T
             MOVE   SPACE     TO  JYO-REC
             INITIALIZE           JYO-REC
     END-IF.
*20230125↑
*------------------------------------------------
     MOVE    ORD-F17            TO    WK-TOKCD.
*------------------------------------------------
*    ↓最後にカウントしなおす
*--- ブレーク時、出荷場所件数マスタ出力
*---       IF   WK-DENNO     NOT =     ORD-A23
*---            PERFORM JSMKENL1-WRT-SEC
*---            ADD     1         TO   CNT-MAISU
*---            MOVE    ORD-A23   TO   WK-DENNO
*---            MOVE    ZERO      TO   CNT-KENSU-D
*---       END-IF
*---       ADD       1            TO   CNT-KENSU-D
*---       ADD       1            TO   CNT-KENSU
*---------------------------------------------------
* ファイル編集・出力
     PERFORM  EDIT-SEC.
* ヨドバシ取込ワークに基幹伝票ＮＯ・行ＮＯを更新
     MOVE     FURISOK      TO   ORD-F18.
     MOVE     OUT-DENNO    TO   ORD-F19.
     MOVE     CNT-GYO      TO   ORD-F20.
     MOVE     "1"          TO   ORD-F23.
     REWRITE  ORD-REC.
*
*
 MAIN-99.
     READ     YODORDL1
              AT END    MOVE      9    TO  END-FG
              NOT AT END
                        ADD       1    TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
     MOVE     SPACE         TO        HEN-REC  JOH-REC  NKK-REC.
     INITIALIZE                       HEN-REC  JOH-REC  NKK-REC.
*
 EDIT-01.
*各量販店領域転送
*  不使用　転送なし
*
 EDIT-02.
*ファイル項目転送
     PERFORM  TENSO-SEC.
*
 EDIT-03.
*当日売上伝票ファイル更新
     WRITE    HEN-REC.
*
 EDIT-04.
*基本情報ファイル更新
     WRITE    JOH-REC.
*
*20230125↓
 EDIT-05.
*ヨドバシ納期変更／出荷確定ファイル出力
     WRITE    NKK-REC.
*20230125↑
*
     ADD      1             TO   WRT-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　　ファイル項目転送                              *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"   TO        S-NAME.
*当日売上伝票ファイルレコード
*取引先コード
     MOVE        ORD-F17   TO        HEN-F01.
*伝票番号
     MOVE        OUT-DENNO TO        HEN-F02
                                     HEN-F23.
*行番号
     MOVE        CNT-GYO   TO        HEN-F03.
*相殺区分
     MOVE        0         TO        HEN-F04.
*取区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*店コード
     MOVE  ORD-F06(1:4)    TO        WK-TEN-F011-2.
     MOVE  WK-TEN-F011-RR  TO        HEN-F07.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09.
*注文番号
     MOVE        SPACE     TO        HEN-F10.
*振分ルート取得
*    MOVE       TBL-F04    TO        WK-RUTO.
*    MOVE       HEN-F02    TO        WK-DENNO.
*---------------------------*
*発注日
     MOVE        ORD-F03   TO        HEN-F111.
*納品日
*20230125↓
*----MOVE   ORD-F14   TO        HEN-F112.
     IF     JYOKEN1-INV-FLG  =  "   "
            MOVE   WK-NPULS-ENCHI   TO      LINK-IN-YMD62
     ELSE
            MOVE   WK-NPULS         TO      LINK-IN-YMD62
     END-IF.
     MOVE   "5"        TO      LINK-IN-KBN.
*XXX MOVE    ORD-F14   TO      LINK-IN-YMD6.
     MOVE    SYS-DATEW TO      LINK-IN-YMD8.
     CALL   "SKYDTCKB" USING   LINK-IN-KBN
                               LINK-IN-YMD6
                               LINK-IN-YMD8
                               LINK-OUT-RET
                               LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   HEN-F112
     ELSE
         MOVE    ZERO           TO   HEN-F112
     END-IF.
*T
**** DISPLAY NC"→納品日　　＝" HEN-F112 UPON CONS.
*T
*20230125↑
*
*出荷日
     MOVE        ZERO      TO        HEN-F113.
*分類コード
*****MOVE        SPACE     TO        HEN-F12.
*#2021/11/29 検品システム側でＰＫリストを分けるため
     MOVE     ORD-F12(5:4) TO        HEN-F12.
*商品区分
     MOVE        SPACE     TO        HEN-F131.
*伝票区分
     MOVE        SPACE     TO        HEN-F132.
*請求
     MOVE        ZERO      TO        HEN-F133.
*伝発区分
     MOVE        9         TO        HEN-F134.
*自社商品コード
     MOVE        TBL-F031  TO        HEN-F1411.
*自社品単コード
     MOVE        TBL-F032  TO        HEN-F1412.
*商品名
     MOVE        MEI-F031  TO        HEN-F1421.
     MOVE        MEI-F032  TO        HEN-F1422.
*数量
     MOVE        ORD-F13   TO        HEN-F15.
*単
     MOVE       "1"        TO        HEN-F16.
*仕入単価
     MOVE        ZERO      TO        HEN-F171.
*原価単価
     MOVE        ORD-F10   TO        HEN-F172.
*売価単価
     MOVE        ZERO      TO        HEN-F173.
*原価金額
     COMPUTE     HEN-F181   =        HEN-F15   *  HEN-F172.
*売価金額
     COMPUTE     HEN-F182   =        HEN-F15   *  HEN-F173.
*消費税
     MOVE        ZERO      TO        HEN-F19.
*粗利
     MOVE        ZERO      TO        HEN-F20.
*ストック番号
     MOVE        SPACE     TO        HEN-F21.
*備考  発注書番号をセットしておく
     MOVE        ORD-F02   TO        HEN-F22.
*自社得意先コード
     MOVE        TOK-F52   TO        HEN-F24.
*相手商品コード
     MOVE        ORD-F08   TO        HEN-F25.
*請求フラグ
     MOVE        ZERO      TO        HEN-F261.
*フラグ2
     MOVE        ZERO      TO        HEN-F262.
*フラグ3
     MOVE        ZERO      TO        HEN-F263.
*フラグ4
     MOVE        ZERO      TO        HEN-F264.
*在庫計上フラグ
     MOVE        ZERO      TO        HEN-F265.
*チェックリスト
     MOVE        ZERO      TO        HEN-F271.
*伝票発行区分
     MOVE        9         TO        HEN-F272.
*修正
     MOVE        ZERO      TO        HEN-F273.
*オンライン区分
     MOVE        1         TO        HEN-F274.
*エントリー区分
     MOVE        1         TO        HEN-F275.
*#2022/02/10 NAV ST
*付番区分
*****MOVE        9         TO        HEN-F276.
     MOVE        0         TO        HEN-F276.
*#2022/02/10 NAV ED
*売上データ作成
     MOVE        ZERO      TO        HEN-F277.
*量販店区分
     MOVE       "A"        TO        HEN-F278.
*端数金額
     MOVE        ZERO      TO        HEN-F279.
*端数消費税
     MOVE        ZERO      TO        HEN-F27A.
*付番修正
     MOVE        ZERO      TO        HEN-F27B.
*在庫引落フラグ
     MOVE        ZERO      TO        HEN-F27C.
*在庫引当フラグ
     MOVE        ZERO      TO        HEN-F27D.
*予備4
     MOVE        ZERO      TO        HEN-F27E.
*予備5
     MOVE        ZERO      TO        HEN-F27F.
*ＷＳ番号
     MOVE        1         TO        HEN-F28.
*変換値
     MOVE        ZERO      TO        HEN-F29.
*店舗名（カナ）
     MOVE        TEN-F04   TO        HEN-F30.
*伝票枚数
     MOVE        ZERO      TO        HEN-F98.
*システム日付
     MOVE        SYS-DATEW TO        HEN-F99.
*オンライン伝票枚数
     MOVE        ZERO      TO        HEN-F40.
*ルート（保管場所）
     MOVE     ORD-F12(5:4) TO        HEN-F42.
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47.
*振分倉庫コード
*    出荷場所特定区分（1:ルート条件マスタ は不使用）
     IF       TOK-F95      =  2
*       明細行番号
        IF       CNT-GYO      =  1
*          出荷場所
           IF      TBL-F04   NOT =  SPACE
*T
*                  DISPLAY "TBL-F02=" TBL-F02 UPON CONS
*                  DISPLAY "TBL-F04=" TBL-F04 UPON CONS
*T
                   MOVE    TBL-F04   TO       FURISOK
           ELSE
                   MOVE    TOK-F81   TO       FURISOK
           END-IF
        ELSE
           CONTINUE
        END-IF
     ELSE
*          代表倉庫
           MOVE  TOK-F81             TO       FURISOK
     END-IF.
     MOVE        FURISOK   TO        HEN-F48.
*棚番
     MOVE        TBL-F08   TO        HEN-F49.
*訂正前数量
     MOVE        ORD-F13   TO        HEN-F50.
*訂正仕入単価
     MOVE        ZERO      TO        HEN-F511.
*修正原価単価
     MOVE        ORD-F10   TO        HEN-F512.
*修正売価単価
     MOVE        ZERO      TO        HEN-F513.
*修正原価金額
     COMPUTE     HEN-F521   =        HEN-F50   *  HEN-F512.
*修正売価金額
     COMPUTE     HEN-F522   =        HEN-F50   *  HEN-F513.
*訂正フラグ
     MOVE        ZERO      TO        HEN-F53.
*更新済フラグ
     MOVE        ZERO      TO        HEN-F54.
*エラーフラグ
     MOVE        ZERO      TO        HEN-F55.
*----------------------------------------------
*----------------------------------------------
*メッセージタイプ
     MOVE    ORD-F01       TO        JOH-F01.
*購買発注番号
     MOVE    ORD-F02       TO        JOH-F02.
*発注伝票日付
     MOVE    ORD-F03       TO        JOH-F03.
*仕入先コード
     MOVE    ORD-F04       TO        JOH-F04.
*発注先コード
     MOVE    ORD-F05       TO        JOH-F05.
*出荷先コード
     MOVE    ORD-F06       TO        JOH-F06.
*明細行番号
     MOVE    ORD-F07       TO        JOH-F07.
*品目番号
     MOVE    ORD-F08       TO        JOH-F08.
*明細フリーテキスト１
     MOVE    ORD-F09       TO        JOH-F09.
*明細単価
     MOVE    ORD-F10       TO        JOH-F10.
*お客様伝票番号
     MOVE    ORD-F11       TO        JOH-F11.
*配送場所
     MOVE    ORD-F12       TO        JOH-F12.
*発注数量
     MOVE    ORD-F13       TO        JOH-F13.
*納入期日
*20230126↓
*----MOVE    ORD-F14       TO        JOH-F14.
     MOVE    HEN-F112      TO        JOH-F14.
*20230126↑
*バッチ（日）
     MOVE    PARA-JDATE    TO        JOH-F15.
*バッチ（時）
     MOVE    PARA-JTIME    TO        JOH-F16.
*バッチ（取）
     MOVE    ORD-F17       TO        JOH-F17.
*基幹伝票番号
     MOVE    OUT-DENNO     TO        JOH-F18.
*基幹行番号
     MOVE    CNT-GYO       TO        JOH-F19.
*取込担当部門CD
     MOVE    ORD-F21       TO        JOH-F20.
*取込担当CD
     MOVE    ORD-F22       TO        JOH-F21.
*作成日
     MOVE    SYS-DATEW     TO        JOH-F22.
*作成時刻
     MOVE    WK-TIME(1:6)  TO        JOH-F23.
*更新日
*    INIT
*更新時刻
*    INIT
*出荷通知作成日
*    INIT
*出荷通知作成時刻
*    INIT
*20230125↓
*振分倉庫
     MOVE    HEN-F48       TO        JOH-F28.
*出荷通知番号
*    INIT
*受注時納品日
     MOVE    ORD-F14       TO        JOH-F30.
*受注時数量
     MOVE    ORD-F13       TO        JOH-F31.
*変更日（納品日）
     MOVE    SYS-DATEW     TO        JOH-F32.
*変更日（数量）
*    INIT
*倉庫確定日
*    INIT
*倉庫確定時刻
*    INIT
*出荷確定ＦＬＧ
*    INIT
*出荷確定日
*    INIT
*商品名
     MOVE    ORD-F24       TO        JOH-F38.
*----------------------------------------------
*20230125↓
*ヨドバシ納期変更／出荷確定ファイル用転送
*区分
     MOVE    "1"           TO        NKK-F01.
*実行日付
     MOVE    SYS-DATEW     TO        NKK-F02.
*実行時刻
     MOVE    WK-TIME(1:6)  TO        NKK-F03.
*バッチ日付
     MOVE    JOH-F15       TO        NKK-F04.
*バッチ時刻
     MOVE    JOH-F16       TO        NKK-F05.
*バッチ取引先
     MOVE    JOH-F17       TO        NKK-F06.
*倉庫
     MOVE    JOH-F28       TO        NKK-F07.
*納品日
     MOVE    JOH-F30       TO        NKK-F08.
*店舗ＣＤ
     MOVE    JOH-F06(1:4)  TO        NKK-F09.
*伝票ＮＯ
     MOVE    JOH-F18       TO        NKK-F10.
*行番号
     MOVE    JOH-F19       TO        NKK-F11.
*変更納品日
     MOVE    JOH-F14       TO        NKK-F12.
*実施担当者ＣＤ
     MOVE    PARA-TANCD    TO        NKK-F61.
*T
**** DISPLAY NC"担当者ＣＤ　＝" PARA-TANCD UPON CONS.
*T
*20230125↑
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT-SEC"  TO   S-NAME.
*
 JSMKENL1-WRT-00.
*初期ＲＥＡＤ
     READ    YODORDL2
             AT  END
                 GO               TO   JSMKENL1-WRT-EXIT
             NOT AT  END
                 MOVE   OR2-F15  TO   BRK-F15
                 MOVE   OR2-F16  TO   BRK-F16
                 MOVE   OR2-F17  TO   BRK-F17
                 MOVE   OR2-F18  TO   BRK-F18
                 MOVE   OR2-F19  TO   BRK-F19
     END-READ.
*
 JSMKENL1-WRT-01.
     MOVE    1                  TO   CNT-MAISU CNT-MAISU-D.
     MOVE    1                  TO   CNT-KENSU.
     MOVE    ZERO               TO   CNT-KENSU-D.
     GO                         TO   JSMKENL1-WRT-03.
*
 JSMKENL1-WRT-02.
*順ＲＥＡＤ
     READ    YODORDL2
             AT  END
                 MOVE  "END"    TO   OR2-END
                 GO             TO   JSMKENL1-WRT-04
             NOT AT END
                 ADD    1       TO   CNT-KENSU
     END-READ.
*
 JSMKENL1-WRT-03.
*当日スケジュールマスタ用
     IF      OR2-F19       NOT =      BRK-F19
             ADD    1       TO        CNT-MAISU
     END-IF.
*T
*    DISPLAY "TOTAL       "           UPON CONS.
*    DISPLAY "  CNT-KENSU=" CNT-KENSU UPON CONS.
*    DISPLAY "  OR2-F19 =" OR2-F19  UPON CONS.
*    DISPLAY "  BRL-FB01 =" BRK-F19  UPON CONS.
*    DISPLAY "  CNT-MAISU=" CNT-MAISU UPON CONS.
*T
*
*件数ファイル用
*T
*    DISPLAY "BETSU       "           UPON CONS.
*    DISPLAY "  OR2-F15 =" OR2-F15  UPON CONS.
*    DISPLAY "  BRK-F15 =" BRK-F15  UPON CONS.
*    DISPLAY "  OR2-F16 =" OR2-F16  UPON CONS.
*    DISPLAY "  BRK-F16 =" BRK-F16  UPON CONS.
*    DISPLAY "  OR2-F17 =" OR2-F17  UPON CONS.
*    DISPLAY "  BRK-F17 =" BRK-F17  UPON CONS.
*    DISPLAY "  OR2-F18 =" OR2-F18  UPON CONS.
*    DISPLAY "  BRK-F18 =" BRK-F18  UPON CONS.
*    DISPLAY "  OR2-F19 =" OR2-F19  UPON CONS.
*    DISPLAY "  BRK-F19 =" BRK-F19  UPON CONS.
*T
     IF    ( OR2-F15   =   BRK-F15 ) AND
           ( OR2-F16   =   BRK-F16 ) AND
           ( OR2-F17   =   BRK-F17 ) AND
           ( OR2-F18   =   BRK-F18 )
             ADD        1          TO       CNT-KENSU-D
             IF         OR2-F19   NOT =    BRK-F19
                        ADD        1        TO   CNT-MAISU-D
                        MOVE       OR2-F19 TO   BRK-F19
             END-IF
*T
*    DISPLAY "  CNT-KENSU-D =" CNT-KENSU-D  UPON CONS
*    DISPLAY "  CNT-MAISU-D =" CNT-MAISU-D  UPON CONS
*T
             GO                   TO   JSMKENL1-WRT-02
     ELSE
             GO                   TO   JSMKENL1-WRT-04
     END-IF.
*
 JSMKENL1-WRT-04.
     MOVE    SPACE        TO        KEN-REC.
     INITIALIZE                     KEN-REC.
     MOVE    BRK-F15      TO        KEN-F01.
     MOVE    BRK-F16      TO        KEN-F02.
     MOVE    BRK-F17      TO        KEN-F03.
     MOVE    BRK-F18      TO        KEN-F04.
     READ    JSMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMKENL1-010
     END-READ.
*
 JSMKENL1-WRT-05.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    BRK-F15      TO        KEN-F01.
     MOVE    BRK-F16      TO        KEN-F02.
     MOVE    BRK-F17      TO        KEN-F03.
     MOVE    BRK-F18      TO        KEN-F04.
*---------------------------*
     MOVE    PARA-KSYU    TO        KEN-F05.
     MOVE    PARA-YUSEN   TO        KEN-F06.
     MOVE    CNT-KENSU-D  TO        KEN-F10.
     MOVE    CNT-MAISU-D  TO        KEN-F11.
     WRITE   KEN-REC.
     MOVE    1            TO        CNT-MAISU-D.
     MOVE    ZERO         TO        CNT-KENSU-D.
     MOVE    OR2-F15      TO        BRK-F15.
     MOVE    OR2-F16      TO        BRK-F16.
     MOVE    OR2-F17      TO        BRK-F17.
     MOVE    OR2-F18      TO        BRK-F18.
     MOVE    OR2-F19      TO        BRK-F19.
     IF      OR2-END       =         "END"
             GO            TO        JSMKENL1-WRT-EXIT
     ELSE
             GO            TO        JSMKENL1-WRT-03
     END-IF.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    CNT-MAISU-D   TO        KEN-F11.
     REWRITE KEN-REC.
     MOVE    1             TO        CNT-MAISU-D.
     MOVE    ZERO          TO        CNT-KENSU-D.
     MOVE    OR2-F15       TO        BRK-F15.
     MOVE    OR2-F16       TO        BRK-F16.
     MOVE    OR2-F17       TO        BRK-F17.
     MOVE    OR2-F18       TO        BRK-F18.
     MOVE    OR2-F19       TO        BRK-F19.
     IF      OR2-END       =         "END"
             GO            TO        JSMKENL1-WRT-EXIT
     ELSE
             GO            TO        JSMKENL1-WRT-03
     END-IF.
*
 JSMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     CLOSE               YODORDL1.
     OPEN      INPUT     YODORDL2.
     IF        WRT-CNT    >      ZERO
*              出荷場所件数マスタ出力
               PERFORM   JSMKENL1-WRT-SEC
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     YODORDL2  JHSHENL1  YODJOHL1
               JSMDAYL1  JSMKENL1  TENMS1
               TOKMS2    SHOTBL1   MEIMS1.
*    ＶＬＤ出力処理
     IF        CNT-MAISU     >    ZERO
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
     CLOSE     VLD500.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL1-WRT-SEC        SECTION.
*
     MOVE   "JSMDAYL1-WRT-SEC"  TO   S-NAME.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD      TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL1-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD      TO        TJS-F03.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     WRITE   TJS-REC.
     GO      TO    JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     REWRITE TJS-REC.
*
 JSMDAYL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD500-OUTPUT-SEC       SECTION.
*
     MOVE   "VLD500-OUTPUT-SEC" TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      PARA-JTIME         TO    VLD-F12.
     MOVE      WK-TOKCD           TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　商品名称マスタ読込　　　　　　　　　　　　*
****************************************************************
 MEIMS1-READ-SEC        SECTION.
*
     MOVE   "MEIMS1-READ-SEC"  TO   S-NAME.
*
     READ   MEIMS1
       INVALID
            MOVE   "INV"   TO        MEIMS1-INV-FLG
       NOT  INVALID
            MOVE   SPACE   TO        MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　条件ファイル　読込　　　　　　　　　　　　*
****************************************************************
 JYOKEN1-READ-SEC        SECTION.
*
     MOVE   "JYOKEN1-READ-SEC"  TO   S-NAME.
*
     READ   JYOKEN1
       INVALID
            MOVE   "INV"   TO        JYOKEN1-INV-FLG
       NOT  INVALID
            MOVE   SPACE   TO        JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
