****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*

*    モジュール名　　　　：　入荷検品データ更新（メーカー発注）*
*    作成日／作成者　　　：　2019/01/18 T.TAKAHASHI            *
*    処理内容　　　　　　：　倉庫入荷確定データを読み、対象発注*
*    　　　　　　　　　　　　データの抽出を行なう。　　　　　　*
*    変更日／作成者　　　：　                                  *
*    変更内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NKE0240B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*倉庫入荷確定ファイル
     SELECT      RCVNYKF     ASSIGN    TO       DA-01-S-RCVNYKXX
                             ACCESS    MODE     SEQUENTIAL
                             FILE      STATUS   RCV-ST.
*
*担当者変換マスタ
     SELECT      TANHENL1    ASSIGN    TO       DA-01-VI-TANHENL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01
                             FILE      STATUS   TAN-ST.
*発注ヘッダファイル
     SELECT      HACHEDL1    ASSIGN    TO       DA-01-VI-HACHEDL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      HED-F02
                             FILE      STATUS   HED-ST.
*発注明細ファイル
     SELECT      HACMEIL1    ASSIGN    TO       DA-01-VI-HACMEIL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     SEQUENTIAL
                             RECORD    KEY      MEI-F02
                                                MEI-F03
                             FILE      STATUS   MEI-ST.
*検品入荷用発注ヘッダファイル
     SELECT      WHCHEDL1    ASSIGN    TO       DA-01-VI-WHCHEDL1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      WCH-F02
                             FILE      STATUS   WCH-ST.
*検品入荷用明細ヘッダファイル
     SELECT      WHCMEIL1  ASSIGN    TO        DA-01-VI-WHCMEIL1
                           ORGANIZATION        INDEXED
                           ACCESS    MODE      RANDOM
                           RECORD    KEY       WCM-F01
                                               WCM-F02
                           FILE      STATUS    WCM-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*倉庫入荷確定ファイル
 FD  RCVNYKF             LABEL RECORD   IS   STANDARD
     BLOCK               CONTAINS       35   RECORDS.
     COPY        RCVNYKF     OF      XFDLIB
                 JOINING     RCV     PREFIX.
*担当者変換マスタ
 FD  TANHENL1.
     COPY        TANHENL1    OF        XFDLIB
     JOINING     TAN         AS        PREFIX.
*発注ヘッダファイル
 FD  HACHEDL1.
     COPY        HACHEDF     OF        XFDLIB
     JOINING     HED         AS        PREFIX.
*発注明細ファイル
 FD  HACMEIL1.
     COPY        HACMEIF     OF        XFDLIB
     JOINING     MEI         AS        PREFIX.
*検品入荷用発注ヘッダファイル
 FD  WHCHEDL1.
     COPY        WHCHEDL1    OF        XFDLIB
     JOINING     WCH         AS        PREFIX.
*検品入荷用明細ヘッダファイル
 FD  WHCMEIL1.
     COPY        WHCMEIL1    OF        XFDLIB
     JOINING     WCM         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  RCV-ST              PIC  X(02)  VALUE  SPACE.
     03  TAN-ST              PIC  X(02)  VALUE  SPACE.
     03  HED-ST              PIC  X(02)  VALUE  SPACE.
     03  MEI-ST              PIC  X(02)  VALUE  SPACE.
     03  WCH-ST              PIC  X(02)  VALUE  SPACE.
     03  WCM-ST              PIC  X(02)  VALUE  SPACE.
 01  SV-AREA.
     03  BR-RCV-F01          PIC  9(07)  VALUE  ZERO.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  RD-CNT              PIC  9(07)  VALUE  ZERO.
     03  TAI-CNT             PIC  9(07)  VALUE  ZERO.
     03  WCH-CNT             PIC  9(07)  VALUE  ZERO.
     03  WCM-CNT             PIC  9(07)  VALUE  ZERO.
     03  SKIP-CNT            PIC  9(07)  VALUE  ZERO.
     03  HACHEDL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  TANHENL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
**
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
*
 01  SYS-DATE8               PIC  9(08)  VALUE  ZERO.
*
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*
 01  SYS-DATE2               PIC  9(08).
 01  FILLER                  REDEFINES  SYS-DATE2.
     03  SYS-YYYY.
         05  SYS-YY2-1       PIC  9(02).
         05  SYS-YY2-2       PIC  9(02).
     03  SYS-MM2             PIC  9(02).
     03  SYS-DD2             PIC  9(02).
*
 01  WK-SAGYOUBI-1           PIC  9(08)  VALUE  ZERO.
 01  WK-SAGYOUBI-1-YMD       REDEFINES   WK-SAGYOUBI-1.
     03  WK-SAGYOUBI-Y       PIC  9(04).
     03  WK-SAGYOUBI-M       PIC  9(02).
     03  WK-SAGYOUBI-D       PIC  9(02).
*
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
 01  FILLER       REDEFINES  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
*
*経理〆日
 01  WK-SIME                 PIC  9(08).
*
*在庫〆日
 01  WK-ZAIKO-SIME.
     03  ZAI-SIME1           PIC  9(08)     VALUE ZERO.
     03  ZAI-SIME1R          REDEFINES      ZAI-SIME1.
         05  ZAI-SIME1R1     PIC  9(06).
         05  ZAI-SIME1R2     PIC  9(02).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  RCV-ERR            PIC  N(10)  VALUE
                   NC"倉庫入荷検品Ｆ　異常".
     03  TAN-ERR            PIC  N(10)  VALUE
                   NC"担当者変換Ｍ　　異常".
     03  HED-ERR             PIC  N(10)  VALUE
                   NC"発注ＨＥＤＦ　　異常".
     03  MEI-ERR             PIC  N(10)  VALUE
                   NC"発注明細　Ｆ　　異常".
     03  WCH-ERR             PIC  N(10)  VALUE
                   NC"ＷＫ発注ヘッダ　異常".
     03  WCM-ERR             PIC  N(10)  VALUE
                   NC"ＷＫ発注明細　　異常".
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NKE0240B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NKE0240B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
*
 01  WK-RUI-F13              PIC  9(08).
 01  FILLER                  REDEFINES  WK-RUI-F13.
     03  WK-RUI-F13-1        PIC  X(04).
     03  WK-RUI-F13-2        PIC  X(02).
     03  WK-RUI-F13-3        PIC  X(02).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*伝票番号サブルーチン用ワーク
 01  OUT-DENNO               PIC 9(07).
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-IN-BUMON               PIC  X(04).
 01  LINK-IN-TANCD               PIC  X(02).
 01  LINK-IN-SOKCD               PIC  X(02).
*01  LINK-OT-KENSU               PIC  9(07).
*01  LINK-OT-TORIDT              PIC  9(08).
*01  LINK-OT-TORITM              PIC  9(06).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-IN-BUMON
                                             LINK-IN-TANCD
                                             LINK-IN-SOKCD.
*                                            LINK-OT-KENSU
*                                            LINK-OT-TORIDT
*                                            LINK-OT-TORITM.
****************************************************************
 DECLARATIVES.
 RCV-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE RCVNYKF.
     DISPLAY     RCV-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     RCV-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 TAN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE TANHENL1.
     DISPLAY     TAN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     TAN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 HED-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HACHEDL1.
     DISPLAY     HED-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     HED-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HACMEIL1.
     DISPLAY     MEI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     MEI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 WCH-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE WHCHEDL1.
     DISPLAY     WCH-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     WCH-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 WHM-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE WHCMEIL1.
     DISPLAY     WCM-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     WCM-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE       "INIT-SEC"   TO   S-NAME.
*
     DISPLAY     MSG-START   UPON  CONS.
*
     OPEN        I-O         WHCHEDL1  WHCMEIL1.
     OPEN        INPUT       RCVNYKF   HACHEDL1  HACMEIL1
                             TANHENL1.
*
*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8
                                       SYS-DATE8.
     DISPLAY "# DATE  = " WK-DATE8     UPON CONS.
     DISPLAY "# TIME  = " WK-TIME-HM   UPON CONS.
*
     MOVE      ZERO               TO   BR-RCV-F01.
*倉庫入荷検品ファイル読込
     PERFORM  RCVNYKF-READ-SEC.
     IF  END-FLG = "END"
          DISPLAY NC"＃＃対象データ無し！！＃＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    倉庫入荷検品ファイル読込　　　　
****************************************************************
 RCVNYKF-READ-SEC            SECTION.
     MOVE "RCVNYKF-READ-SEC"      TO   S-NAME.
*
     READ   RCVNYKF
            AT  END   MOVE "END"  TO   END-FLG
                      GO          TO   RCVNYKF-READ-EXIT
     END-READ.
*
     ADD    1                     TO   RD-CNT.
     IF   RD-CNT(5:3)  =  "000" OR "500"
          DISPLAY "# RD-CNT = " RD-CNT " #" UPON CONS
     END-IF.
*発注区分＝１の対象（社外発注）
     IF   RCV-F10  NOT =  1
          ADD      1              TO   SKIP-CNT
          GO                      TO   RCVNYKF-READ-SEC
     ELSE
          ADD      1              TO   TAI-CNT
     END-IF.
*伝票番号ブレイクチェック
***  DISPLAY "RCV-F01 = " RCV-F01 " * RCV-F02 = " RCV-F02
***          "BR-RCV-F01 = " BR-RCV-F01 UPON CONS.
     IF   BR-RCV-F01  =  RCV-F01
          GO                      TO   RCVNYKF-READ-SEC
     ELSE
          MOVE   RCV-F01          TO   BR-RCV-F01
     END-IF.
*
 RCVNYKF-READ-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
* 発注ヘッダ読込
*****DISPLAY "RCV-F01 = " RCV-F01 " * RCV-F02 = " RCV-F02
*****                                   UPON CONS.
*    発注ヘッダ索引
     MOVE     RCV-F01             TO   HED-F02.
     PERFORM  HACHEDL1-READ-SEC.
     IF  HACHEDL1-INV-FLG  =  "INV"
         DISPLAY NC"＃発注ヘッダ存在無！＃" RCV-F01 UPON CONS
         GO                       TO   MAIN-010
     ELSE
         MOVE    SPACE            TO   WCH-REC
         INITIALIZE                    WCH-REC
         MOVE    HED-REC          TO   WCH-REC
         MOVE    SPACE            TO   WCH-F41
         MOVE  RCV-F19            TO   TAN-F01
         PERFORM  TANHENL1-READ-SEC
         IF  TANHENL1-INV-FLG  =  "INV"
             MOVE "01"            TO   WCH-F36
         ELSE
             MOVE  TAN-F03        TO   WCH-F36
         END-IF
         MOVE      RCV-F04(1:4)   TO   WK-RUI-F13-1
         MOVE      RCV-F04(6:2)   TO   WK-RUI-F13-2
         MOVE      RCV-F04(9:2)   TO   WK-RUI-F13-3
         MOVE      WK-RUI-F13     TO   WCH-F37
         WRITE   WCH-REC
         ADD     1                TO   WCH-CNT
     END-IF.
*    発注明細出力
     MOVE     SPACE               TO   HED-REC.
     INITIALIZE                        HED-REC.
     MOVE     RCV-F01             TO   MEI-F02.
     MOVE     ZERO                TO   MEI-F03.
     START  HACMEIL1  KEY  IS  >=  MEI-F02  MEI-F03
            INVALID
            DISPLAY NC"＃発注明細存在無１！＃" RCV-F01 UPON CONS
            GO                    TO   MAIN-010
     END-START.
*
 MAIN-000.
     READ   HACMEIL1
            AT  END
            GO                    TO   MAIN-010
     END-READ.
*
*****DISPLAY "MEI-F02 = " MEI-F02 " * MEI-F03 = " MEI-F03
*****                                   UPON CONS.
     IF  BR-RCV-F01  NOT =  MEI-F02
         GO                       TO   MAIN-010
     ELSE
         MOVE    SPACE            TO   WCM-REC
         INITIALIZE                    WCM-REC
         MOVE    MEI-F02          TO   WCM-A01
         MOVE    MEI-F03          TO   WCM-A02
*******  DISPLAY "WCM-A01 = " WCM-A01 " WCM-A02 = " WCM-A02
*******          UPON CONS
         MOVE    MEI-REC          TO   WCM-A05
*******  MOVE    RCV-F01          TO   WCM-B01
*******  MOVE    RCV-F02          TO   WCM-B02
*******  MOVE    RCV-F03          TO   WCM-B03
*******  MOVE    RCV-F04          TO   WCM-B04
*******  MOVE    RCV-F05          TO   WCM-B05
*******  MOVE    RCV-F06          TO   WCM-B06(1:8)
*******  MOVE    RCV-F07          TO   WCM-B06(9:5)
*******  MOVE    RCV-F08          TO   WCM-B06(14:2)
*******  MOVE    RCV-F09          TO   WCM-B06(16:1)
*******  MOVE    RCV-F10          TO   WCM-B07
*******  MOVE    RCV-F11          TO   WCM-B08
*******  MOVE    RCV-F12          TO   WCM-B09
*******  MOVE    RCV-F13          TO   WCM-B10
*******  MOVE    RCV-F14          TO   WCM-B11
*******  MOVE    RCV-F15          TO   WCM-B12
*******  MOVE    RCV-F16          TO   WCM-B13
*******  MOVE    RCV-F17          TO   WCM-B14
*******  MOVE    RCV-F18          TO   WCM-B15
*******  MOVE    RCV-F19          TO   WCM-B16
*******  MOVE    RCV-F20          TO   WCM-B17
         WRITE   WCM-REC
         ADD     1                TO   WCM-CNT
         GO                       TO   MAIN-000
     END-IF.
 MAIN-010.
     PERFORM  RCVNYKF-READ-SEC.
*
 MAIN-SEC-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     MOVE     "END-SEC"     TO    S-NAME.
*
     CLOSE    WHCHEDL1  WHCMEIL1  TANHENL1
              RCVNYKF   HACHEDL1  HACMEIL1.
*
     DISPLAY NC"読込　　　件数" " = " RD-CNT   UPON CONS.
     DISPLAY NC"今回対象　件数" " = " TAI-CNT  UPON CONS.
     DISPLAY NC"ＷＫ発注Ｈ件数" " = " WCH-CNT  UPON CONS.
     DISPLAY NC"ＷＫ発注Ｍ件数" " = " WCM-CNT  UPON CONS.
*存在判定
*****IF   WCH-CNT  =  ZERO
*    AND  WCM-CNT  =  ZERO
*         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
*         DISPLAY NC"＃更新対象発注無！！＃" UPON CONS
*         DISPLAY NC"＃　検品側ＤＴの確　＃" UPON CONS
*         DISPLAY NC"＃　認を行なって下　＃" UPON CONS
*         DISPLAY NC"＃　さい。！！！！　＃" UPON CONS
*         DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
*         MOVE    4000      TO    PROGRAM-STATUS
*****END-IF.
*パラメタセット
*****MOVE      TAI-CNT      TO    LINK-OT-KENSU.
*    MOVE      WK-DATE8     TO    LINK-OT-TORIDT.
*****MOVE      WK-TIME-HM   TO    LINK-OT-TORITM.
*
     DISPLAY   MSG-END      UPON  CONS.
*
 END-EXIT.
     EXIT.
***************************************************************
*      ALL       発注ヘッダファイル読込　　                   *
***************************************************************
 HACHEDL1-READ-SEC      SECTION.
*
     MOVE "HACHEDL1-READ-SEC" TO  S-NAME.
*
     READ    HACHEDL1
       INVALID      KEY
          MOVE     "INV"     TO   HACHEDL1-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   HACHEDL1-INV-FLG
     END-READ.
*
 HACHEDL1-READ-EXIT.
     EXIT.
***************************************************************
*      ALL       担当者変換マスタ読込　　　                   *
***************************************************************
 TANHENL1-READ-SEC      SECTION.
*
     MOVE "TANHENL1-READ-SEC" TO  S-NAME.
*
     READ    TANHENL1
       INVALID      KEY
          MOVE     "INV"     TO   TANHENL1-INV-FLG
       NOT INVALID  KEY
          MOVE     SPACE     TO   TANHENL1-INV-FLG
     END-READ.
*
 TANHENL1-READ-EXIT.
     EXIT.
