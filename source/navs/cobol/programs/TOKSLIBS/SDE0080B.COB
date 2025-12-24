****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ取込　　　　　　　　*
*    モジュール名　　　　：　伝票番号採番リストデータ作成　　　*
*    作成日／作成者　　　：　2016/09/28 TAKAHASHI              *
*    処理概要　　　　　　：　採番された伝票番号が、売上伝票Ｆ  *
*                            に存在するかチェックを行なう。　　*
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SDE0080B.
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
*基本売上伝票データ
     SELECT   URIXXXL1  ASSIGN    TO        DA-01-VI-URIXXXL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       URI-F70   URI-F01
                                            URI-F051  URI-F02
                                            URI-F07   URI-F03
                        FILE  STATUS   IS   URI-ST.
*売上伝票ファイル
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F01   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE STATUS    IS   DEN-ST.
*伝票番号採番リストデータ
     SELECT   LSTXXXL1  ASSIGN    TO        DA-01-VI-LSTXXXL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       LST-F01   LST-F02
                                            LST-F03   LST-F05
                        FILE STATUS    IS   LST-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*基本売上伝票データ
 FD  URIXXXL1
                        LABEL RECORD   IS   STANDARD.
     COPY     URIXXXF   OF        XFDLIB
              JOINING   URI  AS   PREFIX.
*売上伝票ファイル
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*伝票番号採番リストデータ
 FD  LSTXXXL1
                        LABEL RECORD   IS   STANDARD.
     COPY     LSTXXXF   OF        XFDLIB
              JOINING   LST  AS   PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  URI-ST              PIC  X(02)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
     03  LST-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  X(03)  VALUE  SPACE.
     03  WK-RD-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-WT-CNT           PIC  9(09)  VALUE  ZERO.
     03  WK-RW-CNT           PIC  9(09)  VALUE  ZERO.
     03  LSTXXXL1-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)  VALUE  SPACE.
*日付取得
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  WK-DATE8.
     03  WK-Y                PIC  9(04)  VALUE  ZERO.
     03  WK-M                PIC  9(02)  VALUE  ZERO.
     03  WK-D                PIC  9(02)  VALUE  ZERO.
*時刻取得
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  URI-ERR             PIC  N(10)  VALUE
                   NC"基本売上伝票ＤＴ異常".
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"売上伝票ファイル異常".
     03  LST-ERR             PIC  N(10)  VALUE
                   NC"伝番採番ＬＤＴ　異常".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*
****************************************************************
 LINKAGE                     SECTION.
****************************************************************
 01  LINK-BUMON                  PIC  X(04).
 01  LINK-TANCD                  PIC  X(02).
****************************************************************
 PROCEDURE                   DIVISION  USING LINK-BUMON
                                             LINK-TANCD.
****************************************************************
 DECLARATIVES.
 URI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE URIXXXL1.
     DISPLAY     URI-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     URI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     DEN-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 LST-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE LSTXXXL1.
     DISPLAY     LST-ERR     UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     LST-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN        I-O         LSTXXXL1
                 INPUT       SHTDENF  URIXXXL1.
*システム日付の取得
     ACCEPT   SYS-DATE          FROM   DATE.
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
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.
*システム時刻の取得
     ACCEPT   SYS-TIME          FROM   TIME.
 INIT-010.
*基本売上伝票データ
     PERFORM  URI-RD-SEC.
*
     IF  END-FLG  =  "END"
         DISPLAY NC"＃＃対象データがありません！！" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.
*売上伝票ファイル索引
     PERFORM SHTDENF-READ-SEC.
*
     PERFORM LST-RD-SEC.
*
     IF  LSTXXXL1-INV-FLG  = "INV"
         MOVE     SPACE           TO   LST-REC
         INITIALIZE                    LST-REC
         MOVE     URI-F01         TO   LST-F01
         MOVE     URI-F051        TO   LST-F02
         MOVE     URI-F07         TO   LST-F03
         MOVE     URI-F112        TO   LST-F04
         MOVE     URI-F02         TO   LST-F05
         MOVE     URI-F03         TO   LST-F06
         IF  SHTDENF-INV-FLG = "INV"
                  MOVE   SPACE    TO   LST-F94
         ELSE
                  MOVE   "1"      TO   LST-F94
         END-IF
         MOVE     URI-F70         TO   LST-F95
         MOVE     URI-F62         TO   LST-F96
         MOVE     URI-F67         TO   LST-F97
         MOVE     LINK-BUMON      TO   LST-F98
         MOVE     LINK-TANCD      TO   LST-F99
         WRITE  LST-REC
         ADD      1               TO   WK-WT-CNT
     ELSE
         IF  URI-F03  =  80
             MOVE     "1"             TO   LST-F93
         ELSE
             MOVE     URI-F03         TO   LST-F06
         END-IF
         IF  SHTDENF-INV-FLG = "INV"
                  MOVE   SPACE    TO   LST-F94
         ELSE
                  MOVE   "1"      TO   LST-F94
         END-IF
         REWRITE  LST-REC
         ADD      1               TO   WK-RW-CNT
     END-IF.
*基本売上伝票データ
 URI-READ.
     PERFORM  URI-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*               基本売上伝票データ読込
****************************************************************
 URI-RD-SEC                 SECTION.
     MOVE     "URI-RD-SEC"        TO   S-NAME.
*
     READ     URIXXXL1  AT   END
              MOVE     "END"      TO   END-FLG
              NOT  AT  END
              ADD       1         TO   WK-RD-CNT
     END-READ.
*
 URI-RD-EXIT.
     EXIT.
****************************************************************
*               伝票番号採番リストデータ読込（計上確認）
****************************************************************
 LST-RD-SEC                  SECTION.
     MOVE     "LST-RD-SEC"        TO   S-NAME.
*
     MOVE     URI-F01        TO   LST-F01.
     MOVE     URI-F051       TO   LST-F02.
     MOVE     URI-F07        TO   LST-F03.
     MOVE     URI-F02        TO   LST-F05.
     READ     LSTXXXL1
              INVALID
              MOVE  "INV"    TO   LSTXXXL1-INV-FLG
              NOT INVALID
              MOVE  SPACE    TO   LSTXXXL1-INV-FLG
     END-READ.
*
 LST-RD-EXIT.
     EXIT.
****************************************************************
*                売上伝票ファイル読込
****************************************************************
 SHTDENF-READ-SEC          SECTION.
*
     MOVE      "SHTDENF-READ-SEC" TO    S-NAME.
*
     MOVE      URI-F01     TO     DEN-F01.
     MOVE      URI-F02     TO     DEN-F02.
     MOVE      URI-F04     TO     DEN-F04.
     MOVE      URI-F051    TO     DEN-F051.
     MOVE      URI-F07     TO     DEN-F07.
     MOVE      URI-F112    TO     DEN-F112.
     MOVE      URI-F03     TO     DEN-F03.
*
     READ      SHTDENF
               INVALID
               MOVE      "INV"    TO    SHTDENF-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    SHTDENF-INV-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
*
     DISPLAY "READ-CNT    = " WK-RD-CNT  UPON  CONS.
     DISPLAY "WRITE-CNT   = " WK-WT-CNT  UPON  CONS.
     DISPLAY "REWRITE-CNT = " WK-RW-CNT  UPON  CONS.
*
     CLOSE URIXXXL1 SHTDENF LSTXXXL1.
*
 END-EXIT.
     EXIT.
