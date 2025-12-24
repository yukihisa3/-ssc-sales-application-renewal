# NJH7906B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/NJH7906B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ作成　　　　　　　*
*    作成日／更新日　　　：　06/06/08                          *
*    作成者／更新者　　　：　NAV MATSUNO                       *
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*　　　　　　　　　　　　　　ダイキ　　　　　　　　　　　　　　*
*　                                                            *
*    2007/05/17 ＤＣＭＪＡＰＡＮ対応　　　 NAV MATSUNO         *
*　  2008/01/11 伝票番号の頭３桁を店舗下３桁に変更する       *
*　  2009/07/30 取引先ＣＤ変更対応　複数取引先対応　　　     *
*　  2013/01/10 ルートマスタ対応　　　　　　　　　　　　　　 *
*　  2013/08/15 発注種別区分追加（改廃）　　　　　　　　　　 *
*　  2017/06/06 発注種別区分追加（５１、７１、８１、８３）　 *
*　                                                            *
*    作成日／更新日　　　：　2019/03/18                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注種別変換マスタ対応　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH7906B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          06/06/08.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*変換伝票データ
     SELECT   JHSHENL1  ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY                 HEN-F46
                                            HEN-F47   HEN-F01
                                            HEN-F02   HEN-F03
                        FILE  STATUS   IS   HEN-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*出荷場所件数マスタ
     SELECT   JSMKENL1  ASSIGN    TO        DA-01-VI-JSMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*当日スケジュールマスタ
     SELECT   JSMDAYL1  ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
*#2019/03/18 NAV ST
*発注種別変換マスタ
     SELECT   DCMHSBL2  ASSIGN    TO        DA-01-VI-DCMHSBL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HSB-F01  HSB-F03
                        FILE  STATUS   IS   HSB-STATUS.
*#2019/03/18 NAV ED
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　１２８　  ＢＦ＝　３０
******************************************************************
 FD  CVCSG001
************************BLOCK CONTAINS      30   RECORDS
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01.
         05  DEN-01A             PIC  X(01).
         05  DEN-01B             PIC  X(127).
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
******************************************************************
*    出荷場所別件数ファイル
******************************************************************
 FD  JSMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     DISHIRED  OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
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
*#2019/03/18 NAV ST
******************************************************************
*    発注種別変換マスタ
******************************************************************
 FD  DCMHSBL2           LABEL RECORD   IS   STANDARD.
     COPY     DCMHSBF   OF        XFDLIB
              JOINING   HSB       PREFIX.
*
*#2019/03/18 NAV ED
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D1            PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D2            PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU1              PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU1              PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU2              PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU2              PIC  9(08)     VALUE  ZERO.
 01  CNT-DENNO               PIC  9(09)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(08)     VALUE  ZERO.
 01  WK-TOKCD1               PIC  9(08)     VALUE  ZERO.
 01  WK-TOKCD2               PIC  9(08)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-RUTO                 PIC  X(02)     VALUE  SPACE.
 01  WK-TOKCD1-CHK           PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD2-CHK           PIC  9(01)     VALUE  ZERO.
*ルート変換
 01  HEN-RUT.
     03  HEN-RUT-1           PIC  X(01)     VALUE  "0".
     03  HEN-RUT-2           PIC  X(01)     VALUE  SPACE.
*#2019/03/18 NAV ST
 01  DCMHSBL2-INV-FLG        PIC  X(03)     VALUE  SPACE.
*#2019/03/18 NAV ED
*
*ファイルヘッドレコード退避ワーク
 01  WK-DEPA-REC.
     03  WK-DEPA01          PIC  X(01).
     03  WK-DEPA02          PIC  9(02).
     03  WK-DEPA03          PIC  9(06).
     03  WK-DEPA04          PIC  9(06).
     03  WK-DEPA05          PIC  9(06).
     03  WK-DEPA06          PIC  9(06).
     03  WK-DEPA07          PIC  9(02).
     03  WK-DEPA08          PIC  9(06).
     03  WK-DEPA09          PIC  9(02).
     03  WK-DEPA10          PIC  9(06).
     03  WK-DEPA11          PIC  9(02).
     03  WK-DEPA12          PIC  9(03).
     03  WK-DEPA13          PIC  9(06).
     03  WK-DEPA14          PIC  9(05).
     03  WK-DEPA15          PIC  X(69).
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(02).
     03  WK-DEPB03          PIC  9(09).
     03  WK-DEPB04          PIC  9(04).
     03  WK-DEPB05          PIC  9(05).
     03  WK-DEPB06          PIC  9(04).
     03  WK-DEPB07          PIC  9(02).
     03  WK-DEPB08          PIC  9(06).
     03  WK-DEPB09          PIC  9(06).
     03  WK-DEPB10          PIC  9(06).
     03  WK-DEPB11          PIC  9(02).
     03  WK-DEPB12          PIC  X(15).
     03  WK-DEPB13          PIC  X(15).
     03  WK-DEPB14          PIC  X(15).
     03  WK-DEPB15          PIC  X(15).
     03  WK-DEPB16          PIC  9(06).
     03  WK-DEPB17          PIC  9(04).
*#2017/06/06 NAV ST 新発注種別追加の為
*****03  WK-DEPB18          PIC  X(10).
     03  WK-DEPB18.
         05  WK-DEPB181     PIC  X(02).
         05  WK-DEPB182     PIC  X(02).
         05  WK-DEPB183     PIC  X(01).
         05  WK-DEPB184     PIC  X(03).
         05  WK-DEPB185     PIC  X(01).
         05  WK-DEPB186     PIC  X(01).
*#2017/06/06 NAV ED
     03  WK-DEPB19          PIC  X(01).
*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).
     03  WK-DEPD02          PIC  9(02).
     03  WK-DEPD03          PIC  9(02).
     03  WK-DEPD04          PIC  9(13).
     03  WK-DEPD05          PIC  9(04).
     03  WK-DEPD06          PIC  9(04).
     03  WK-DEPD07          PIC  X(02).
     03  WK-DEPD08          PIC  9(05)V9(01).
     03  WK-DEPD09          PIC  9(07)V9(02).
     03  WK-DEPD10          PIC  9(07).
     03  WK-DEPD11          PIC  9(10).
     03  WK-DEPD12          PIC  9(10).
     03  WK-DEPD13          PIC  X(09).
     03  WK-DEPD14          PIC  X(20).
     03  WK-DEPD15          PIC  X(20).
     03  WK-DEPD16          PIC  X(01).
     03  WK-DEPD17          PIC  X(03).
     03  WK-DEPD18          PIC  9(03).
*#2017/06/06 NAV ST
*****03  WK-DEPD19          PIC  X(02).
     03  WK-DEPD191         PIC  X(01).
     03  WK-DEPD192         PIC  X(01).
*#2017/06/06 NAV ED
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  HDJ-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
*#2019/03/18 NAV ST
     03  HSB-STATUS        PIC  X(02).
*#2019/03/18 NAV ED
*取引先ＣＤ変換（文字⇒数値）
 01  WK-TORICD             PIC  X(05).
 01  WK-TORICD-R           REDEFINES  WK-TORICD.
     03  WK-HEN-TORICD     PIC  9(05).
*
 01  WK-DENNO-H            PIC  X(07).
 01  WK-DENNO-H-R          REDEFINES  WK-DENNO-H.
     03  WK-HEN-DENNO-H    PIC  9(07).
*数量
 01  WK-SURYO              PIC S9(08).
 01  WK-SURYO-R            REDEFINES  WK-SURYO.
     03  WK-HEN-SURYO      PIC S9(06)V9(02).
*原価
 01  WK-GENKA              PIC S9(10).
 01  WK-GENKA-R            REDEFINES  WK-GENKA.
     03  WK-HEN-GENKA      PIC S9(08)V9(02).
*金額
 01  WK-YEN                PIC S9(11).
*伝票番号編集
 01  WK-DEN-NO.
     03  WK-DEN-TEN        PIC 9(03)     VALUE  ZERO.
     03  WK-DEN-DEN        PIC 9(06)     VALUE  ZERO.
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH7906B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH7906B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH7906B".
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
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE         PIC   9(08).
 01  PARA-JTIME         PIC   9(04).
 01  PARA-KSYU          PIC   X(01).
 01  PARA-YUSEN         PIC   X(01).
 01  PARA-TOKCD1        PIC   9(08).
 01  PARA-TOKCD2        PIC   9(08).
 01  PARA-TOKCK1        PIC   X(01).
 01  PARA-TOKCK2        PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-KSYU
                                       PARA-YUSEN
                                       PARA-TOKCD1
                                       PARA-TOKCD2
                                       PARA-TOKCK1
                                       PARA-TOKCK2.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
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
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
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
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1"     TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
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
 FILEERR-SEC7           SECTION.
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
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
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
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*#2019/03/18 NAV ST
*
 FILEERR-SEC12          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMHSBL2.
     MOVE      "DCMHSBL2"   TO   AB-FILE.
     MOVE      HSB-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*#2019/03/18 NAV ED
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
     OPEN     INPUT     CVCSG001
                        SHOTBL1   MEIMS1
                        JHMRUTL1  TOKMS2  TENMS1.
 INIT-010.
     OPEN     EXTEND    JHSHENL1.
 INIT-020.
     OPEN     I-O       JSMKENL1  JSMDAYL1.
 INIT-030.
     OPEN     OUTPUT    VLD500.
*#2019/03/18 NAV ST
     OPEN     INPUT     DCMHSBL2.
*#2019/03/18 NAV ED
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
 INIT-040.
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
         MOVE    ZERO             TO   SYS-DATEW
     END-IF.
 INIT-050.
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO    S-NAME.
*
     IF    DEN-01A    =    "A"
           MOVE      SPACE       TO    WK-DEPA-REC
           INITIALIZE                  WK-DEPA-REC
           MOVE      DEN-01      TO    WK-DEPA-REC
     END-IF.
*伝票ヘッダ処理
     IF    DEN-01A    =    "B"
*          出荷場所件数マスタ出力
           IF    CNT-MAISU1   >    ZERO
                 PERFORM  JSMKENL1-WRT-SEC
           END-IF
           IF    CNT-MAISU2   >    ZERO
                 PERFORM  JSMKENL1-WRT1-SEC
           END-IF
           MOVE      SPACE       TO    WK-DEPB-REC
           INITIALIZE                  WK-DEPB-REC
           MOVE      DEN-01      TO    WK-DEPB-REC
           ADD       1           TO    CNT-MAISU
           IF        WK-DEPB10  =  100403
                     ADD    1    TO    CNT-MAISU1
           END-IF
           IF        WK-DEPB10  =  100441
                     ADD    1    TO    CNT-MAISU2
           END-IF
           MOVE      ZERO        TO    CNT-KENSU-D
           MOVE      ZERO        TO    CNT-KENSU-D1
           MOVE      ZERO        TO    CNT-KENSU-D2
     END-IF.
*明細行
     IF   DEN-01A    =    "D"
          MOVE      DEN-01      TO    WK-DEPD-REC
          ADD       1           TO    CNT-KENSU
          ADD       1           TO    CNT-KENSU-D
           IF        WK-DEPB10  =  100403
                     ADD    1    TO    CNT-KENSU1
                     ADD    1    TO    CNT-KENSU-D1
           END-IF
           IF        WK-DEPB10  =  100441
                     ADD    1    TO    CNT-KENSU2
                     ADD    1    TO    CNT-KENSU-D2
           END-IF
*         変換データ 出力
          PERFORM  EDIT-SEC
     END-IF.
*
     READ   CVCSG001
         AT END
            MOVE     9           TO    END-FG
         NOT AT END
            ADD      1           TO    RD-CNT
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
     MOVE     SPACE         TO        HEN-REC.
     INITIALIZE                       HEN-REC.
*各量販店領域転送
*伝票ヘッダー転送
*    レコード区分
     MOVE     WK-DEPB01     TO        HEN-A01.
*    データ種別
     MOVE     WK-DEPB02     TO        HEN-A02.
*    伝票番号　
     MOVE     WK-DEPB03     TO        HEN-A03.
*    法人コード
     MOVE     WK-DEPB04     TO        HEN-A04.
*    店舗コード
     MOVE     WK-DEPB05     TO        HEN-A05.
*    分類コード
     MOVE     WK-DEPB06     TO        HEN-A06.
*    伝票区分
     MOVE     WK-DEPB07     TO        HEN-A07.
*    発注日
     MOVE     WK-DEPB08     TO        HEN-A08.
*    納品日　
     MOVE     WK-DEPB09     TO        HEN-A09.
*    取引先コード
     MOVE     WK-DEPB10     TO        HEN-A10.
*    ＳＡ　　　
     MOVE     WK-DEPB11     TO        HEN-A11.
*    仕入先名（上段）
     MOVE     WK-DEPB12     TO        HEN-A12.
*    仕入先名（下段）
     MOVE     WK-DEPB13     TO        HEN-A13.
*    発注企業名
     MOVE     WK-DEPB14     TO        HEN-A14.
*    店舗名
     MOVE     WK-DEPB15     TO        HEN-A15.
*    納品指定日
     MOVE     WK-DEPB16     TO        HEN-A16.
*    納品指定時間　　
     MOVE     WK-DEPB17     TO        HEN-A17.
*    コメント
*#2017/06/06 NAV ST
*****MOVE     WK-DEPB18     TO        HEN-A18.
     MOVE     WK-DEPB181    TO        HEN-A181.
     MOVE     WK-DEPB182    TO        HEN-A182.
     MOVE     WK-DEPB183    TO        HEN-A183.
     MOVE     WK-DEPB184    TO        HEN-A184.
     MOVE     WK-DEPB185    TO        HEN-A185.
     MOVE     WK-DEPB186    TO        HEN-A186.
*#2017/06/06 NAV ED
*    発注種別区分
     MOVE     WK-DEPB19     TO        HEN-A19.
*伝票明細転送
*    レコード区分
     MOVE     WK-DEPD01     TO        HEN-A20.
*    データ種別
     MOVE     WK-DEPD02     TO        HEN-A21.
*    伝票行番号
     MOVE     WK-DEPD03     TO        HEN-A22.
*    ＪＡＮＣＤ
     MOVE     WK-DEPD04     TO        HEN-A23.
*    入数
     MOVE     WK-DEPD05     TO        HEN-A24.
*    発注単位数　　
     MOVE     WK-DEPD06     TO        HEN-A25.
*    単位　　
     MOVE     WK-DEPD07     TO        HEN-A26.
*    発注数量
     MOVE     WK-DEPD08     TO        HEN-A27.
*    原価単価
     MOVE     WK-DEPD09     TO        HEN-A28.
*    売価単価
     MOVE     WK-DEPD10     TO        HEN-A29.
*    原価金額
     MOVE     WK-DEPD11     TO        HEN-A30.
*    売価金額
     MOVE     WK-DEPD12     TO        HEN-A31.
*    取引先商品ＣＤ
     MOVE     WK-DEPD13     TO        HEN-A32.
*    商品名
     MOVE     WK-DEPD14     TO        HEN-A33.
*    規格名　　
     MOVE     WK-DEPD15     TO        HEN-A34.
*    計上区分　　
     MOVE     WK-DEPD16     TO        HEN-A35.
*    発注単位
     MOVE     WK-DEPD17     TO        HEN-A36.
*    中分類ＣＤ
     MOVE     WK-DEPD18     TO        HEN-A37.
*#2017/06/06 NAV ST
*    仕入先ロケーション
*****MOVE     WK-DEPD19     TO        HEN-A38.
     MOVE     WK-DEPD191    TO        HEN-A381.
     MOVE     SPACE         TO        HEN-A382.
*#2017/06/06 NAV ED
*#2017/06/06 NAV ST
     MOVE     HEN-A182      TO        HEN-A39.
     MOVE     HEN-A381      TO        HEN-A40.
     MOVE     HEN-A35       TO        HEN-A41.
     MOVE     HEN-A185      TO        HEN-A42.
     MOVE     HEN-A183      TO        HEN-A43.
     MOVE     HEN-A184      TO        HEN-A44.
     MOVE     HEN-A05       TO        HEN-A45.
*#2017/06/06 NAV ED
*****DISPLAY  " WK-DEPA01 = " WK-DEPA01 UPON CONS.
*****DISPLAY  " WK-DEPA02 = " WK-DEPA02 UPON CONS.
*****DISPLAY  " WK-DEPA03 = " WK-DEPA03 UPON CONS.
*****DISPLAY  " WK-DEPA04 = " WK-DEPA04 UPON CONS.
*****DISPLAY  " WK-DEPA05 = " WK-DEPA05 UPON CONS.
*****DISPLAY  " WK-DEPA06 = " WK-DEPA06 UPON CONS.
*****DISPLAY  " WK-DEPA07 = " WK-DEPA07 UPON CONS.
*****DISPLAY  " WK-DEPA08 = " WK-DEPA08 UPON CONS.
*****DISPLAY  " WK-DEPA09 = " WK-DEPA09 UPON CONS.
*****DISPLAY  " WK-DEPA10 = " WK-DEPA10 UPON CONS.
*****DISPLAY  " WK-DEPA11 = " WK-DEPA11 UPON CONS.
*****DISPLAY  " WK-DEPA12 = " WK-DEPA12 UPON CONS.
*****DISPLAY  " WK-DEPA13 = " WK-DEPA13 UPON CONS.
*
     PERFORM  TENSO-SEC.
*
     WRITE    HEN-REC.
     ADD      1             TO   WRT-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変換伝票転送                                    *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"         TO        S-NAME.
*取引先コード
*2009/07/30 データ内の取引先ＣＤを使用する。
     MOVE    HEN-A10             TO        HEN-F01.
     MOVE    HEN-A10             TO        WK-TOKCD.
     IF      WK-TOKCD = 100403
             MOVE      1         TO        WK-TOKCD1-CHK
                                           PARA-TOKCK1
             MOVE      WK-TOKCD  TO        WK-TOKCD1  PARA-TOKCD1
     END-IF.
     IF      WK-TOKCD = 100441
             MOVE      1         TO        WK-TOKCD2-CHK
                                           PARA-TOKCK2
             MOVE      WK-TOKCD  TO        WK-TOKCD2  PARA-TOKCD2
     END-IF.
*****MOVE    100403              TO        HEN-F01.
*****MOVE    100403              TO        WK-TOKCD.
*伝票ナンバー
*2008/01/11 伝票番号の頭３桁に店番を付加
*****MOVE    HEN-A03             TO        HEN-F02
*****                                      HEN-F23.
     MOVE    HEN-A05             TO        WK-DEN-TEN.
     MOVE    HEN-A03             TO        WK-DEN-DEN.
     MOVE    WK-DEN-NO           TO        HEN-F02
                                           HEN-F23.
*行番号
     MOVE    HEN-A22             TO        HEN-F03.
*伝区
     MOVE        40              TO        HEN-F051.
     MOVE     NC"売上伝票"       TO        HEN-F052.
*担当者コード
     MOVE        99              TO        HEN-F06.
*店コード
     MOVE    HEN-A05             TO        HEN-F07.
*****COMPUTE  HEN-F07  =  4000   +   HEN-A05.
*店舗マスタ検索（センター区分）
     MOVE        SPACE           TO        TEN-REC.
     INITIALIZE                            TEN-REC.
     MOVE        HEN-F01         TO        TEN-F52.
     MOVE        HEN-F07         TO        TEN-F011.
     READ    TENMS1
       INVALID
         MOVE    SPACE           TO        TEN-REC
         INITIALIZE                        TEN-REC
     END-READ.
*発注種別区分
     MOVE         HEN-A19        TO        HEN-F27F.
*センター区分
     IF  TEN-F75  =  "1"
         MOVE    1               TO        HEN-F40
*####### EVALUATE  HEN-A19
*#######     WHEN  "0"  MOVE "ﾃｲﾊﾞﾝ"       TO   HEN-F42(1:5)
*2013/08/15 NAV ST "ｼﾝｷ  "=> "ｶｲﾊｲ "に変更
*************WHEN  "1"  MOVE "ｼﾝｷ  "       TO   HEN-F42(1:5)
*#######     WHEN  "1"  MOVE "ｶｲﾊｲ "       TO   HEN-F42(1:5)
*2013/08/15 NAV ED "ｼﾝｷ  "=> "ｶｲﾊｲ "に変更
*#######     WHEN  "2"  MOVE "ｼﾝﾃﾝ "       TO   HEN-F42(1:5)
*#######     WHEN  "3"  MOVE "ﾁﾗｼ  "       TO   HEN-F42(1:5)
*#######     WHEN  "4"  MOVE "ｵｸﾘｺﾐ"       TO   HEN-F42(1:5)
*#######     WHEN  "5"  MOVE "ｿﾞｳｼｮ"       TO   HEN-F42(1:5)
*#######     WHEN  "6"  MOVE "ｷｬｸﾁｭ"       TO   HEN-F42(1:5)
*************2017/06/06 NAV ST
*#######     WHEN  "7"  MOVE "ｼｮｳｶﾝ"       TO   HEN-F42(1:5)
*#######     WHEN  "8"  MOVE "HBﾎｼﾞ"       TO   HEN-F42(1:5)
*#######     WHEN  "9"  MOVE "ﾋﾞﾋﾝﾖ"       TO   HEN-F42(1:5)
*#######     WHEN  "A"  MOVE "ﾌﾟﾛﾓｰ"       TO   HEN-F42(1:5)
*#######     WHEN  "B"  MOVE "BYｶｲﾊ"       TO   HEN-F42(1:5)
*************2017/06/06 NAV ED
*#######     WHEN OTHER MOVE "********"    TO   HEN-F42(1:5)
*####### END-EVALUATE
*#2019/03/18 NAV ST 発注種別変換マスタより取得
         MOVE     HEN-F01                  TO   HSB-F01
         MOVE     HEN-A19                  TO   HSB-F03
         PERFORM  DCMHSBL2-READ-SEC
         IF  DCMHSBL2-INV-FLG  =  SPACE
                  MOVE   HSB-F05           TO   HEN-F42(1:5)
         ELSE
                  MOVE   "*****"           TO   HEN-F42(1:5)
         END-IF
*#2019/03/18 NAV ED
         EVALUATE  HEN-A35
             WHEN  "0"  MOVE "0:ﾅｼ"        TO   HEN-F42(6:5)
             WHEN  "1"  MOVE "1:ｺﾓﾉ"       TO   HEN-F42(6:5)
             WHEN  "2"  MOVE "2:ｲｹｲ"       TO   HEN-F42(6:5)
             WHEN  "3"  MOVE "3:ｹｰｽ"       TO   HEN-F42(6:5)
             WHEN  "4"  MOVE "4:ｿﾉﾀ"       TO   HEN-F42(6:5)
             WHEN OTHER MOVE "********"    TO   HEN-F42(6:5)
         END-EVALUATE
     ELSE
         MOVE    ZERO            TO        HEN-F40
         MOVE " :ﾃﾝﾎﾟﾁｮｸｿｳ"      TO        HEN-F42
     END-IF.
*  商品変換テーブル検索（自社ＣＤで検索）
     MOVE        SPACE           TO        TBL-REC.
     INITIALIZE                            TBL-REC.
     MOVE        HEN-F01         TO        TBL-F01.
*****MOVE        HEN-A32(2:8)    TO        TBL-F02.
     MOVE        HEN-A23         TO        TBL-F02.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE           TO        TBL-REC
         INITIALIZE                        TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04         TO        HEN-F08
                                           HEN-F09.
*発注日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE       "3"              TO        LINK-IN-KBN.
     MOVE        HEN-A08         TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"       USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8   TO   HEN-F111
     ELSE
         MOVE    ZERO            TO   HEN-F111
     END-IF.
*納品日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE       "3"              TO        LINK-IN-KBN.
     MOVE        HEN-A16         TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"       USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD8.
     IF          LINK-OUT-RET    =     ZERO
         MOVE    LINK-OUT-YMD8   TO        HEN-F112
     ELSE
         MOVE    ZERO            TO        HEN-F112
     END-IF.
*****MOVE        HEN-F112        TO        HEN-F113.
*分類（部門）
     MOVE        HEN-A06         TO        HEN-F12.
*商品区分　　
     MOVE        SPACE           TO        HEN-F131.
*伝票区分　　
     MOVE        HEN-A07         TO        HEN-F132.
*伝発区分
     MOVE        9               TO        HEN-F134.
*自社商品コード
     MOVE        TBL-F031        TO        HEN-F1411.
*自社商品単品コード
     MOVE        TBL-F032        TO        HEN-F1412.
*商品名　　　
*    商品名称マスタ検索
     MOVE        SPACE           TO        MEI-REC.
     INITIALIZE                            MEI-REC.
     MOVE        TBL-F031        TO        MEI-F011.
     MOVE        TBL-F032        TO        MEI-F012.
     READ    MEIMS1
       INVALID
*        商品名
         MOVE    HEN-A33(1:15)   TO   HEN-F1421
         MOVE    HEN-A33(16:5)   TO   HEN-F1422(1:5)
         MOVE    HEN-A34(1:10)   TO   HEN-F1422(6:10)
       NOT INVALID
         MOVE    HEN-A33(1:15)   TO   HEN-F1421
         MOVE    HEN-A33(16:5)   TO   HEN-F1422(1:5)
         MOVE    HEN-A34(1:10)   TO   HEN-F1422(6:10)
     END-READ.
*数量
     MOVE        HEN-A27         TO        HEN-F15.
*単
     MOVE       "1"              TO        HEN-F16.
*原価単価
*****DISPLAY "TBL-F05 = " TBL-F05  UPON CONS.
     MOVE        HEN-A28         TO        HEN-F172.
*売価単価
     MOVE        HEN-A29         TO        HEN-F173.
*原価金額
     COMPUTE     HEN-F181  =  HEN-A27  *  HEN-A28.
*売価金額
     COMPUTE     HEN-F182  =  HEN-A27  *  HEN-A29.
*****MOVE        HEN-A32         TO        HEN-F182.
*店舗名（備考）
     MOVE        HEN-A15         TO        HEN-F22.
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE           TO        TOK-REC
         INITIALIZE                        TOK-REC
         MOVE    HEN-F01         TO        TOK-F01
         READ    TOKMS2
             INVALID
               MOVE  SPACE       TO        TOK-REC
               INITIALIZE                  TOK-REC
         END-READ
         MOVE    TOK-F52         TO        HEN-F24
         MOVE    1               TO        FLG-TOK
     ELSE
         MOVE    TOK-F52         TO        HEN-F24
     END-IF.
*相手商品コード
*****MOVE        HEN-A32(2:8)    TO        HEN-F25.
     MOVE        HEN-A23         TO        HEN-F25.
*伝票発行区分
     MOVE        9               TO        HEN-F272.
*オンライン区分
     MOVE        1               TO        HEN-F274.
*エントリー区分
     MOVE        1               TO        HEN-F275.
*付番区分
     MOVE        9               TO        HEN-F276.
*量販店区分
     MOVE       "A"              TO        HEN-F278.
*ＷＳ■
     MOVE        1               TO        HEN-F28.
*変換値
*****MOVE        WK-NEN          TO        HEN-F29.
*店舗名（カナ）
     MOVE        HEN-A15         TO        HEN-F30.
*システム日付
     MOVE        SYS-DATEW       TO        HEN-F99.
*ルート（直送区分）
*****MOVE        HEN-A38         TO        HEN-F42.
*受信日付
     MOVE     PARA-JDATE         TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME         TO        HEN-F47.
*振分倉庫コード
*ルート条件マスタ検索
     MOVE     ZERO               TO        INV-RUT.
     MOVE     SPACE              TO        RUT-REC.
     INITIALIZE                            RUT-REC.
*取引先ＣＤは取引先エリアの取引先ＣＤを使用すること
     MOVE     HEN-F01            TO        RUT-F01.
     MOVE     SPACE              TO        RUT-F02.
*****MOVE     HEN-A13            TO        RUT-F03.
*2013/01/10 NAV ST ルートセット変更
*****MOVE     "00"               TO        RUT-F03.
*#2017/06/06 NAV ST
*****MOVE     WK-DEPB18(1:2)     TO        RUT-F03.
     MOVE     WK-DEPB181         TO        RUT-F03.
*#2017/06/06 NAV ED
*2013/01/10 NAV ED ルートセット変更
     READ     JHMRUTL1
         INVALID
           MOVE  1               TO        INV-RUT
           MOVE  TOK-F81         TO        HEN-F48
         NOT INVALID
           MOVE  RUT-F05         TO        HEN-F48
     END-READ.
*_番
     MOVE        TBL-F08         TO        HEN-F49.
*訂正前数量
     MOVE        HEN-A27         TO        HEN-F50.
*修正原価単価
     MOVE        HEN-A28         TO        HEN-F512.
*修正売価単価
     MOVE        HEN-A29         TO        HEN-F513.
*修正原価金額
     MOVE        HEN-F181        TO        HEN-F521.
*修正売価金額
     MOVE        HEN-F182        TO        HEN-F522.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-TOKCD1     TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
     READ    JSMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMKENL1-010
     END-READ.
*
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-TOKCD1     TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D1  TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JSMKENL1-WRT-EXIT.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     ADD     CNT-KENSU-D1  TO        KEN-F10.
     ADD     1             TO        KEN-F11.
     REWRITE KEN-REC.
*
 JSMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT1-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT1-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-TOKCD2     TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
     READ    JSMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMKENL1-110
     END-READ.
*
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    WK-TOKCD2     TO        KEN-F03.
     IF      INV-RUT       =         ZERO
             MOVE  RUT-F05     TO    KEN-F04
     ELSE
             MOVE  TOK-F81     TO    KEN-F04
     END-IF.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D2  TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JSMKENL1-WRT1-EXIT.
*
 JSMKENL1-110.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     ADD     CNT-KENSU-D2  TO        KEN-F10.
     ADD     1             TO        KEN-F11.
     REWRITE KEN-REC.
*
 JSMKENL1-WRT1-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        CNT-MAISU1    >    ZERO
*              出荷場所件数マスタ出力
               PERFORM   JSMKENL1-WRT-SEC
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT1-SEC
     END-IF.
     IF        CNT-MAISU2    >    ZERO
*              出荷場所件数マスタ出力
               PERFORM   JSMKENL1-WRT1-SEC
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT2-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     CVCSG001  JHSHENL1
               SHOTBL1   MEIMS1
               JHMRUTL1  TOKMS2  TENMS1
               JSMKENL1  JSMDAYL1.
*#2019/03/18 NAV ST
     CLOSE     DCMHSBL2.
*#2019/03/18 NAV ED
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU1    >    ZERO
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
     IF        CNT-MAISU2    >    ZERO
               PERFORM   VLD500-OUTPUT1-SEC
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
 JSMDAYL1-WRT1-SEC       SECTION.
*
     MOVE   "JSMDAYL1-WRT1-SEC" TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD1     TO        TJS-F03.
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
     MOVE    WK-TOKCD1     TO        TJS-F03.
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU1    TO        TJS-F09.
     MOVE    CNT-MAISU1    TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     WRITE   TJS-REC.
     GO      TO    JSMDAYL1-WRT1-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU1    TO        TJS-F09.
     MOVE    CNT-MAISU1    TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     REWRITE TJS-REC.
*
 JSMDAYL1-WRT1-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL1-WRT2-SEC       SECTION.
*
     MOVE   "JSMDAYL1-WRT2-SEC" TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD2     TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL1-210
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    WK-TOKCD2     TO        TJS-F03.
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU2    TO        TJS-F09.
     MOVE    CNT-MAISU2    TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     WRITE   TJS-REC.
     GO      TO    JSMDAYL1-WRT2-EXIT.
*
 JSMDAYL1-210.
*
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU2    TO        TJS-F09.
     MOVE    CNT-MAISU2    TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     REWRITE TJS-REC.
*
 JSMDAYL1-WRT2-EXIT.
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
     MOVE      WK-TOKCD1          TO    VLD-F13.
     DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
     DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
     DISPLAY "WK-TOKCD1  = " WK-TOKCD1  UPON CONS.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD500-OUTPUT1-SEC      SECTION.
*
     MOVE   "VLD500-OUTPUT1-SEC"  TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      PARA-JTIME         TO    VLD-F12.
     MOVE      WK-TOKCD2          TO    VLD-F13.
     DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
     DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
     DISPLAY "WK-TOKCD2  = " WK-TOKCD2  UPON CONS.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT1-EXIT.
     EXIT.
****************************************************************
*　　発注種別変換マスタ索引
****************************************************************
 DCMHSBL2-READ-SEC         SECTION.
*
     READ     DCMHSBL2
         INVALID
           MOVE  "INV"     TO        DCMHSBL2-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        DCMHSBL2-INV-FLG
     END-READ.
*
 DCMHSBL2-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
