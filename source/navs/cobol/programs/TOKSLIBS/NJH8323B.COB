****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　変換伝票データ作成　　　　　　　　*
*    作成日／更新日　　　：　2000/10/10                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*                            ホーマック　　　　　　　　　　　　*
*　　　　　　　　　　　　　　（ホーマック北海道）　　　　　　　*
*    作成日／更新日　　　：　2014/08/18                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注納品変更方法対応　　　　　　　*
*　                                                            *
*    作成日／更新日　　　：　2018/01/26                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　形状区分＝４扱い変更（大カゴ）　　*
*　                                                            *
*    作成日／更新日　　　：　2019/02/21                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注種別区分対応　　　　　　　　　*
*　                                                            *
*    作成日／更新日　　　：　2019/03/11                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注種別変換マスタ対応　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH8323B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/10/10.
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
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*#2019/03/11 NAV ST
*発注種別変換マスタ
     SELECT   DCMHSBL2  ASSIGN    TO        DA-01-VI-DCMHSBL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HSB-F01  HSB-F03
                        FILE  STATUS   IS   HSB-STATUS.
*#2019/03/11 NAV ED
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  PIC  X(01).
     03  DEN-02                  PIC  9(01).
     03  DEN-03                  PIC  X(126).
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
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
     COPY     HMSHIRED  OF        XFDLIB
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
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*#2019/03/11 NAV ST
******************************************************************
*    発注種別変換マスタ
******************************************************************
 FD  DCMHSBL2           LABEL RECORD   IS   STANDARD.
     COPY     DCMHSBL2  OF        XFDLIB
              JOINING   HSB       PREFIX.
*
*#2019/03/11 NAV ED
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
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  WK-TOKCD                PIC  9(06)     VALUE  ZERO.
 01  WK-KEY                  PIC  X(01)     VALUE  SPACE.
 01  WK-KEY-FLG              PIC  X(01)     VALUE  SPACE.
 01  FURIWAKE-CD             PIC  9(01)     VALUE  ZERO.
 01  DAIHYO-BASYO-CD         PIC  X(02)     VALUE  SPACE.
 01  SYUKA-BASYO             PIC  X(02)     VALUE  SPACE.
*##2014/0428 NAV ST
 01  WK-RUTO-CD              PIC  X(02)     VALUE  SPACE.
*##2014/0428 NAV ED
*##2014/08/18 NAV ST 発注納品方法変更対応
 01  JHMRUTF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*##2014/08/18 NAV ED
*#2019/03/11 NAV ST
 01  DCMHSBL2-INV-FLG        PIC  X(03)     VALUE  SPACE.
*#2019/03/11 NAV ED
*出荷場所退避
 01  WK-BASYO.
     03  WK-SYUKA-BASYO      PIC  X(02)     VALUE  SPACE.
*ルート変換
 01  HEN-RUT.
     03  HEN-RUT-1           PIC  X(01)     VALUE  "0".
     03  HEN-RUT-2           PIC  X(01)     VALUE  SPACE.
*
*ヘッドレコード退避ワーク
 01  WK-DEPB-REC.
     03  WK-DEPB01          PIC  X(01).
     03  WK-DEPB02          PIC  9(01).
     03  WK-DEPB03          PIC  9(07).
     03  WK-DEPB04          PIC  9(06).
     03  WK-DEPB05          PIC  9(06).
     03  WK-DEPB06.
         05  WK-DEPB061     PIC  9(06).
         05  WK-DEPB062     PIC  9(02).
     03  WK-DEPB07.
         05  WK-DEPB071     PIC  X(20).
         05  WK-DEPB072     PIC  X(20).
     03  WK-DEPB08.
         05  WK-DEPB081     PIC  9(04).
         05  WK-DEPB082     PIC  X(02).
     03  WK-DEPB09          PIC  X(15).
     03  WK-DEPB10.
         05  WK-DEPB101     PIC  9(03).
         05  WK-DEPB102     PIC  X(01).
     03  WK-DEPB11          PIC  9(02).
     03  WK-DEPB12          PIC  9(04).
     03  WK-DEPB13          PIC  9(02).
     03  WK-DEPB14          PIC  9(01).
     03  WK-DEPB15          PIC  9(01).
*2014/08/18 NAV ST 発注納品方法変更対応
*****03  WK-DEPB16          PIC  X(23).
     03  WK-DEPB16          PIC  X(18).
     03  WK-DEPB161         PIC  X(02).
     03  WK-DEPB162         PIC  X(01).
     03  WK-DEPB163         PIC  X(01).
     03  WK-DEPB164         PIC  X(01).
*2014/08/18 NAV ED
     03  WK-DEPB17          PIC  X(01).

*    明細レコード退避ワーク
 01  WK-DEPD-REC.
     03  WK-DEPD01          PIC  X(01).
     03  WK-DEPD02          PIC  9(01).
     03  WK-DEPD03.
         05  WK-DEPD031     PIC  9(07).
         05  WK-DEPD032     PIC  X(01).
     03  WK-DEPD04.
         05  WK-DEPD041     PIC  X(20).
         05  WK-DEPD042     PIC  X(20).
     03  WK-DEPD05          PIC  9(05)V9.
     03  WK-DEPD06          PIC  9(06)V99.
     03  WK-DEPD07          PIC  9(08).
     03  WK-DEPD08          PIC  9(06).
     03  WK-DEPD09          PIC  9(08).
     03  WK-DEPD10          PIC  X(13).
*2014/08/18 NAV ST 発注納品方法変更対応
*****03  WK-DEPD11          PIC  X(29).
     03  WK-DEPD11          PIC  X(27).
     03  WK-DEPD111         PIC  X(01).
     03  WK-DEPD112         PIC  X(01).
*2014/08/18 NAV ED 発注納品方法変更対応
*    合計レコード退避ワーク
 01  WK-DEPT-REC.
     03  WK-DEPT01          PIC  X(01).
     03  WK-DEPT02          PIC  9(01).
     03  WK-DEPT03          PIC  9(08).
     03  WK-DEPT04          PIC  X(08).
     03  WK-DEPT05          PIC  X(110).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
*#2019/03/11 NAV ST
     03  HSB-STATUS        PIC  X(02).
*#2019/03/11 NAV ED
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH8323B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH8323B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH8323B".
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
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
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
     MOVE      "TENMS1"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*#2019/03/11 NAV ST
*
 FILEERR-SEC11          SECTION.
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
*#2019/03/11 NAV ED
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
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JSMKENL1  JSMDAYL1.
     OPEN     OUTPUT    VLD500.
*#2019/03/11 NAV ST
     OPEN     INPUT     DCMHSBL2.
*#2019/03/11 NAV ED
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WK-DEPB-REC.
     INITIALIZE                   WK-DEPB-REC.
     MOVE     SPACE     TO        WK-DEPD-REC.
     INITIALIZE                   WK-DEPD-REC.
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
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
     MOVE     DEN-01             TO   WK-KEY.
*    伝票ヘッダレコード
     IF    DEN-01  =  "H"
*          出荷場所件数マスタ出力
           IF   CNT-MAISU     >    ZERO
                PERFORM  JSMKENL1-WRT-SEC
           END-IF
           MOVE      SPACE    TO    WK-DEPB-REC
           INITIALIZE               WK-DEPB-REC
           MOVE      DEN-REC  TO    WK-DEPB-REC
           ADD       1        TO   CNT-MAISU
           MOVE      ZERO     TO   CNT-KENSU-D
     END-IF.
*明細行
     IF    DEN-01  =  "L"
           MOVE      SPACE    TO   WK-DEPD-REC
           INITIALIZE              WK-DEPD-REC
           MOVE      DEN-REC  TO   WK-DEPD-REC
           ADD       1        TO   CNT-KENSU
           ADD       1        TO   CNT-KENSU-D
     END-IF.
*
     READ     CVCSG001
              AT END
              MOVE     9      TO    END-FG
              GO              TO    MAIN-EXIT
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*データＫＥＹが”Ｔ”の場合、レコード内に合計セット
     IF    DEN-01  =  "T"
           MOVE       "T"     TO    WK-KEY-FLG
           MOVE      SPACE    TO    WK-DEPT-REC
           INITIALIZE               WK-DEPT-REC
           MOVE      DEN-REC  TO    WK-DEPT-REC
*****DISPLAY "WK-DEPB1 = " WK-DEPB-REC UPON CONS
*****DISPLAY "WK-DEPD1 = " WK-DEPD-REC UPON CONS
           PERFORM   EDIT-SEC
     ELSE
*          一つ前のＫＥＹが”Ｌ”の場合
           IF  WK-KEY  =  "L"
               MOVE      SPACE    TO    WK-KEY-FLG
               PERFORM   EDIT-SEC
           END-IF
     END-IF.
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
*****DISPLAY "WK-DEPB = " WK-DEPB-REC UPON CONS.
*    ヘッダＫＥＹ１
     MOVE     WK-DEPB01     TO        HEN-A01.
*    ヘッダＫＥＹ２
     MOVE     WK-DEPB02     TO        HEN-A02.
*    注文番号
     MOVE     WK-DEPB03     TO        HEN-A03.
*    発注日
     MOVE     WK-DEPB04     TO        HEN-A04.
*    入荷予定日
     MOVE     WK-DEPB05     TO        HEN-A05.
*    取引先ＣＤ／直送
     MOVE     WK-DEPB061    TO        HEN-A061.
     MOVE     WK-DEPB062    TO        HEN-A062.
*    取引先名称（１）、（２）
     MOVE     WK-DEPB071    TO        HEN-A071.
     MOVE     WK-DEPB072    TO        HEN-A072.
*    店舗ＣＤ
     MOVE     WK-DEPB081    TO        HEN-A081.
     MOVE     WK-DEPB082    TO        HEN-A082.
*    店舗名
     MOVE     WK-DEPB09     TO        HEN-A09.
*    部門ＣＤ
     MOVE     WK-DEPB101    TO        HEN-A101.
     MOVE     WK-DEPB102    TO        HEN-A102.
*    伝票区分
     MOVE     WK-DEPB11     TO        HEN-A11.
*    連番
     MOVE     WK-DEPB12     TO        HEN-A12.
*    法人ＣＤ
     MOVE     WK-DEPB13     TO        HEN-A13.
*    発注区分
     MOVE     WK-DEPB14     TO        HEN-A14.
*    納品区分
     MOVE     WK-DEPB15     TO        HEN-A15.
     MOVE     WK-DEPB16     TO        HEN-A16.
*2014/08/18 NAV ST 発注納品方法変更対応
*    発注種別区分
     MOVE     WK-DEPB161    TO        HEN-A161.
*    発注種別変換区分
     MOVE     WK-DEPB164    TO        HEN-A164.
*2014/08/18 NAV ED
*    センター受領データ区分
     MOVE     WK-DEPB17     TO        HEN-A17.
*    明細ＫＥＹ１
     MOVE     WK-DEPD01     TO        HEN-A18.
*    明細ＫＥＹ２
     MOVE     WK-DEPD02     TO        HEN-A19.
*    商品ＣＤ
     MOVE     WK-DEPD031    TO        HEN-A201.
     MOVE     WK-DEPD032    TO        HEN-A202.
*    商品名称（１）、（２）
     MOVE     WK-DEPD041    TO        HEN-A211.
     MOVE     WK-DEPD042    TO        HEN-A212.
*    発注数量
     MOVE     WK-DEPD05     TO        HEN-A22.
*    原価単価
     MOVE     WK-DEPD06     TO        HEN-A23.
*    原価金額
     MOVE     WK-DEPD07     TO        HEN-A24.
*    売価単価
     MOVE     WK-DEPD08     TO        HEN-A25.
*    売価金額
     MOVE     WK-DEPD09     TO        HEN-A26.
*    ＪＡＮＣＤ
     MOVE     WK-DEPD10     TO        HEN-A27.
     MOVE     WK-DEPD11     TO        HEN-A28.
*2014/08/18 NAV ST 発注納品方法変更対応
*    発注単位区分
     MOVE     WK-DEPD111    TO        HEN-A162.
*    形状区分
     MOVE     WK-DEPD112    TO        HEN-A163.
*2014/08/18 NAV ED 発注納品方法変更対応
     IF       WK-KEY-FLG  =  "T"
*             合計ＫＥＹ１
              MOVE     WK-DEPT01     TO        HEN-A29
*             合計ＫＥＹ２
              MOVE     WK-DEPT02     TO        HEN-A30
*             原価金額合計
              MOVE     WK-DEPT03     TO        HEN-A31
*             売価金額合計
              MOVE     WK-DEPT04     TO        HEN-A32
              MOVE     WK-DEPT05     TO        HEN-A33
     ELSE
*             合計ＫＥＹ１
              MOVE     SPACE         TO        HEN-A29
*             合計ＫＥＹ２
              MOVE     ZERO          TO        HEN-A30
*             原価金額合計
              MOVE     ZERO          TO        HEN-A31
*             売価金額合計
              MOVE     ZERO          TO        HEN-A32
              MOVE     SPACE         TO        HEN-A33
     END-IF.
*****IF       WK-KEY-FLG = "T"
*****         DISPLAY "HEN-F01  = " HEN-F01 UPON CONS
*****END-IF.
     PERFORM  TENSO-SEC.
*
*****IF       WK-KEY-FLG = "T"
*****         DISPLAY "HEN-REC2 = " HEN-REC UPON CONS
*****END-IF.
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
     MOVE    "TENSO-SEC"   TO        S-NAME.
*取引先コード
*****MOVE        HEN-A061  TO        HEN-F01.
*****MOVE        HEN-A061  TO        WK-TOKCD.
     MOVE        883       TO        HEN-F01.
     MOVE        883       TO        WK-TOKCD.
*伝票ナンバー
     MOVE        HEN-A03   TO        HEN-F02.
     MOVE        HEN-F02   TO        HEN-F23.
*行_
     MOVE        HEN-A19   TO        HEN-F03.
*伝区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*店コード
     MOVE        HEN-A081  TO        HEN-F07.
*2014/08/18 NAV ST 発注納品方法変更対応
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
*センター区分
     IF  TEN-F75  =  "1"
         MOVE    1               TO        HEN-F40
*####### EVALUATE  HEN-A164
*#######     WHEN  "0"  MOVE "ﾃｲﾊﾞﾝ"       TO   HEN-F42(1:5)
*#######     WHEN  "2"  MOVE "ｼﾝﾃﾝ "       TO   HEN-F42(1:5)
*#######     WHEN  "3"  MOVE "ﾄｸﾊﾞｲ"       TO   HEN-F42(1:5)
*#######     WHEN  "4"  MOVE "ﾄｳﾆｭｳ"       TO   HEN-F42(1:5)
*#2019/02/21 NAV ST
*#######     WHEN  "6"  MOVE "KKﾀﾞｲ"       TO   HEN-F42(1:5)
*#######     WHEN  "F"  MOVE "FYﾀﾞｲ"       TO   HEN-F42(1:5)
*#######     WHEN  "A"  MOVE "ﾎﾝﾌﾞﾎ"       TO   HEN-F42(1:5)
*#######     WHEN  "B"  MOVE "ｾﾝﾖｳﾀ"       TO   HEN-F42(1:5)
*#######     WHEN  "C"  MOVE "ﾖｳﾄﾞﾋ"       TO   HEN-F42(1:5)
*#######     WHEN  "D"  MOVE "ｶｲﾊｲ "       TO   HEN-F42(1:5)
*#######     WHEN  "E"  MOVE "BYｶｲﾊ"       TO   HEN-F42(1:5)
*#2019/02/21 NAV ED
*#######     WHEN OTHER MOVE "********"    TO   HEN-F42(1:5)
*####### END-EVALUATE
*#2019/03/11 NAV ST 発注種別変換マスタより取得
         MOVE     HEN-F01                  TO   HSB-F01
         MOVE     HEN-A164                 TO   HSB-F03
         PERFORM  DCMHSBL2-READ-SEC
*********DISPLAY "DSP => " HEN-F01 " : " HEN-A164
*********" : " DCMHSBL2-INV-FLG UPON CONS
         IF  DCMHSBL2-INV-FLG  =  SPACE
                  MOVE   HSB-F05           TO   HEN-F42(1:5)
         ELSE
                  MOVE   "*****"           TO   HEN-F42(1:5)
         END-IF
*#2019/03/11 NAV ED
         EVALUATE  HEN-A163
             WHEN  "0"  MOVE "0:ﾅｼ"        TO   HEN-F42(6:5)
             WHEN  "1"  MOVE "1:ｺﾓﾉ"       TO   HEN-F42(6:5)
             WHEN  "2"  MOVE "2:ｲｹｲ"       TO   HEN-F42(6:5)
             WHEN  "3"  MOVE "3:ｹｰｽ"       TO   HEN-F42(6:5)
*#2018/01/26 NAV ST
*************WHEN  "4"  MOVE "4:ｿﾉﾀ"       TO   HEN-F42(6:5)
             WHEN  "4"  MOVE "4:ｶｺﾞ"       TO   HEN-F42(6:5)
*#2018/01/26 NAV ED
             WHEN OTHER MOVE "********"    TO   HEN-F42(6:5)
         END-EVALUATE
*********発注単位区分が"C"ケースの場合は、全てケースに集約
         IF  HEN-A162 = "C"
             MOVE "3:ｹｰｽ"                  TO   HEN-F42(6:5)
         END-IF
     ELSE
         MOVE    ZERO            TO        HEN-F40
         MOVE " :ﾃﾝﾎﾟﾁｮｸｿｳ"      TO        HEN-F42
     END-IF.
*2014/08/18 NAV ED
*  商品変換テーブル検索
     MOVE        SPACE     TO        TBL-REC.
     INITIALIZE                      TBL-REC.
     MOVE        HEN-F01   TO        TBL-F01.
*****MOVE        HEN-A201  TO        TBL-F02.
     MOVE        HEN-A27   TO        TBL-F02.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
         INITIALIZE                  TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09.
*１行目の出荷場所ＣＤを退避する。
     IF  HEN-F03  =  1
         MOVE  TBL-F04     TO        SYUKA-BASYO
     END-IF.
*発注日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
*****COMPUTE     HEN-F111  = WK-NEN * 10000 + HEN-A07.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        HEN-A04   TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   HEN-F111
     ELSE
         MOVE    ZERO           TO   HEN-F111
     END-IF.
*納品日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
*****COMPUTE     HEN-F112  = WK-NEN * 10000 + HEN-A08.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        HEN-A05   TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   HEN-F112
     ELSE
         MOVE    ZERO           TO   HEN-F112
     END-IF.
*****MOVE        HEN-F112  TO        HEN-F113.
*分類（部門）
     MOVE        HEN-A101  TO        HEN-F12.
*商品区分　　
     MOVE        SPACE     TO        HEN-F131.
*伝票区分　　
     MOVE        HEN-A11   TO        HEN-F132.
*伝発区分
     MOVE        9         TO        HEN-F134.
*自社商品コード
     MOVE        TBL-F031  TO        HEN-F1411.
*自社商品単品コード
     MOVE        TBL-F032  TO        HEN-F1412.
*商品名　　　
*    商品名称マスタ検索
     MOVE        SPACE     TO        MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE        TBL-F031  TO        MEI-F011.
     MOVE        TBL-F032  TO        MEI-F012.
     READ    MEIMS1
       INVALID
*        商品名
         MOVE    HEN-A211(1:15) TO   HEN-F1421
         MOVE    HEN-A211(16:5) TO   HEN-F1422(1:5)
         MOVE    HEN-A212(1:10) TO   HEN-F1422(6:10)
       NOT INVALID
         MOVE    HEN-A211(1:15) TO   HEN-F1421
         MOVE    HEN-A211(16:5) TO   HEN-F1422(1:5)
         MOVE    HEN-A212(1:10) TO   HEN-F1422(6:10)
     END-READ.
*数量
     MOVE        HEN-A22   TO        HEN-F15.
*単
     MOVE       "1"        TO        HEN-F16.
*原価単価
     MOVE        HEN-A23   TO        HEN-F172.
*売価単価
     MOVE        HEN-A25   TO        HEN-F173.
*原価金額
     MOVE        HEN-A24   TO        HEN-F181.
*売価金額
     MOVE        HEN-A26   TO        HEN-F182.
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    HEN-F01       TO    TOK-F01
         READ    TOKMS2
             INVALID
               MOVE  SPACE     TO    TOK-REC
               INITIALIZE            TOK-REC
         END-READ
         MOVE    TOK-F52   TO        HEN-F24
*2014/08/18 NAV ST 発注納品方法変更対応
*********振分倉庫ＣＤ／代表場所ＣＤ
         MOVE    TOK-F95   TO        FURIWAKE-CD
         MOVE    TOK-F81   TO        DAIHYO-BASYO-CD
*2014/08/18 NAV ED
         MOVE    1         TO        FLG-TOK
     ELSE
         MOVE    TOK-F52   TO        HEN-F24
     END-IF.
*相手商品コード
*****MOVE        HEN-A201  TO        HEN-F25.
     MOVE        HEN-A27   TO        HEN-F25.
*振分倉庫／ルートコード
*  出荷場所振分方法を特定する。
     EVALUATE   FURIWAKE-CD
         WHEN   1
*          *ルート条件マスタよりセット
*          *マスタ非存在時は取引先マスタの代表倉庫をセット
             PERFORM  JHMRUTF-READ-SEC
             IF  JHMRUTF-INV-FLG = "INV"
                 MOVE  DAIHYO-BASYO-CD  TO  HEN-F48
             ELSE
                 MOVE  RUT-F05          TO  HEN-F48
             END-IF
         WHEN   2
*          *伝票１行目商品(商品変換TBL)よりセット
*          *ＴＢＬ非存在時は取引先マスタの代表倉庫をセット
             IF  SYUKA-BASYO     NOT =  SPACE
                 MOVE  SYUKA-BASYO      TO  HEN-F48
             ELSE
                 MOVE  DAIHYO-BASYO-CD  TO  HEN-F48
             END-IF
         WHEN   3
*          *取引先マスタの代表倉庫をセット
             MOVE  DAIHYO-BASYO-CD  TO   HEN-F48
         WHEN   OTHER
*          *それ以外→取引先マスタの代表倉庫をセット
             MOVE  DAIHYO-BASYO-CD  TO   HEN-F48
     END-EVALUATE.
*振分倉庫ＣＤを退避する
     MOVE  HEN-F48         TO        WK-SYUKA-BASYO.
*伝票発行区分
     MOVE        9         TO        HEN-F272.
*オンライン区分
     MOVE        1         TO        HEN-F274.
*エントリー区分
     MOVE        1         TO        HEN-F275.
*付番区分
     MOVE        9         TO        HEN-F276.
*量販店区分
     MOVE       "A"        TO        HEN-F278.
*ＷＳ_
     MOVE        1         TO        HEN-F28.
*変換値
*****MOVE        WK-NEN    TO        HEN-F29.
*店舗名（カナ）
     MOVE        HEN-A09   TO        HEN-F30.
*システム日付
     MOVE        SYS-DATEW TO        HEN-F99.
*2014/08/18 NAV ST 発注納品方法変更対応　コメントアウト
*ルート（直送区分）
*####MOVE        HEN-A062  TO        HEN-F42.
*2014/08/18 NAV ED
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47.
*振分倉庫コード
*2014/08/18 NAV ST 発注納品方法変更対応　コメントアウト
*    ルート条件マスタ検索
*##  MOVE     ZERO         TO        INV-RUT.
*##  MOVE     SPACE        TO        RUT-REC.
*##  INITIALIZE                      RUT-REC.
*取引先ＣＤは取引先エリアの取引先ＣＤを使用すること
*****MOVE     HEN-F01      TO        RUT-F01.
*##  MOVE     HEN-F01      TO        RUT-F01.
*##  MOVE     SPACE        TO        RUT-F02.
*##  MOVE     HEN-A062     TO        RUT-F03.
*##  READ     JHMRUTL1
*##      INVALID
*##        MOVE  1         TO        INV-RUT
*##        MOVE  TOK-F81   TO        HEN-F48
*##      NOT INVALID
*##        MOVE  RUT-F05   TO        HEN-F48
*##  END-READ.
*_番
     MOVE        TBL-F08   TO        HEN-F49.
*訂正前数量
     MOVE        HEN-A22   TO        HEN-F50.
*修正原価単価
     MOVE        HEN-A23   TO        HEN-F512.
*修正売価単価
     MOVE        HEN-A25   TO        HEN-F513.
*修正原価金額
     MOVE        HEN-A24   TO        HEN-F521.
*修正売価金額
     MOVE        HEN-A26   TO        HEN-F522.
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
     MOVE    WK-TOKCD      TO        KEN-F03.
     MOVE    WK-SYUKA-BASYO TO       KEN-F04.
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
     MOVE    WK-TOKCD      TO        KEN-F03.
     MOVE    WK-SYUKA-BASYO TO       KEN-F04.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JSMKENL1-WRT-EXIT.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     ADD     CNT-KENSU-D   TO        KEN-F10.
     ADD     1             TO        KEN-F11.
     REWRITE KEN-REC.
*
 JSMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        CNT-MAISU     >    ZERO
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
     CLOSE     CVCSG001  JHSHENL1
               SHOTBL1   MEIMS1
               JHMRUTL1  TOKMS2  TENMS1
               JSMKENL1  JSMDAYL1.
*#2019/03/11 NAV ST
     CLOSE     DCMHSBL2.
*#2019/03/11 NAV ED
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
               DISPLAY "AAA" UPON CONS
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
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     WRITE   TJS-REC.
     GO      TO    JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
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
     DISPLAY "PARA-JDATE = " PARA-JDATE UPON CONS.
     DISPLAY "PARA-JTIME = " PARA-JTIME UPON CONS.
     DISPLAY "WK-TOKCD   = " WK-TOKCD   UPON CONS.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
****************************************************************
*　　ルート条件マスタ索引
****************************************************************
 JHMRUTF-READ-SEC          SECTION.
*
     MOVE     HEN-F01      TO        RUT-F01.
     MOVE     SPACE        TO        RUT-F02.
     MOVE     HEN-A062     TO        RUT-F03.
     READ     JHMRUTL1
         INVALID
           MOVE  "INV"     TO        JHMRUTF-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        JHMRUTF-INV-FLG
     END-READ.
*
 JHMRUTF-READ-EXIT.
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
