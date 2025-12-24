# SJK0030B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJK0030B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品　　　　　　　　　　　*
*    モジュール名　　　　：　受信自動欠品処理　　　　　　　　　*
*    作成日／更新日　　　：　2015/10/09(FRI)                   *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*                        ：　受信時刻     PARA-TIME            *
*                        ：　納品日       PARA-NOBI            *
*    処理概要　　　　　　：　売上伝票Ｆを読み、出荷制限Ｍを参  *
*                        ：　照して出荷数を０にする。　　　　  *
*                        ：　処理結果を自動欠品結果Ｆに更新す  *
*                        ：　る。　　　　　　　　　　　　　　  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJK0030B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/10/09.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
                        STATUS         SHTDENF-ST.
*----<< 出荷制限マスタ >>--*
     SELECT   SYUSGNF   ASSIGN         DA-01-VI-SYUSGNL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SYU-F01
                                       SYU-F02
                                       SYU-F03
                        STATUS         SYUSGNF-ST.
*----<< 自動欠品結果ファイル >>--*
     SELECT   JIDKEKF   ASSIGN         DA-01-VI-JIDKEKLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JID-F46
                                       JID-F47
                                       JID-F01
                                       JID-F48
                                       JID-F02
                                       JID-F04
                                       JID-F051
                                       JID-F07
                                       JID-F112
                                       JID-F03
                        STATUS         JIDKEKF-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 出荷制限マスタ >>--*
 FD  SYUSGNF            LABEL RECORD   IS   STANDARD.
     COPY     SYUSGNF   OF        XFDLIB
              JOINING   SYU       PREFIX.
*----<< 自動欠品結果ファイル >>--*
 FD  JIDKEKF            LABEL     RECORD   IS   STANDARD.
     COPY     JIDKEKF   OF        XFDLIB
              JOINING   JID       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENF-ST        PIC  X(02).
 01  SYUSGNF-ST        PIC  X(02).
 01  JIDKEKF-ST        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  SYU-INVALID-FLG   PIC  X(01).
 01  JID-INVALID-FLG   PIC  X(01).
*
*----<< FLAG >>--*
 01  TAISHO-FLG        PIC  X(01).
*
*----<< ｶｳﾝﾄ >>--*
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
*----<< 在庫更新サブルーチン用 >>--*
 01  ZAIKO-UPDATE-AREA.
     03  ZAIKO-MODE          PIC  X(01)     VALUE  "1".
     03  ZAIKO-TORICD        PIC  9(08).
     03  ZAIKO-AITESHOHINCD  PIC  X(13).
     03  ZAIKO-SOKOCD        PIC  X(02).
     03  ZAIKO-SHOHINCD      PIC  X(08).
     03  ZAIKO-HINTANCD      PIC  X(08).
     03  ZAIKO-NOHINBI       PIC  9(08).
     03  HENKOMAE-SURYO      PIC  9(09).
     03  HENKOGO-SURYO       PIC  9(09)     VALUE  ZERO.
     03  ZAIKO-TANABAN       PIC  X(06).
     03  ZAIKO-HIKIATE-FLG   PIC  X(01).
*
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJK0030B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0030B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0030B".
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
 LINKAGE                SECTION.
 01  PARA-TORICD            PIC   9(08).
 01  PARA-DATE              PIC   9(08).
 01  PARA-TIME              PIC   9(04).
 01  PARA-NOBI              PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-TORICD
                                        PARA-DATE
                                        PARA-TIME
                                        PARA-NOBI.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 売上伝票ファイル >>--*
 SHTDENF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     MOVE      "SHTDENF"    TO   AB-FILE.
     MOVE      SHTDENF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 出荷制限マスタ >>--*
 SYUSGNF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYUSGNF.
     MOVE      "SYUSGNF"    TO   AB-FILE.
     MOVE      SYUSGNF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 自動欠品結果ファイル >>--*
 JIDKEKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JIDKEKF.
     MOVE      "JIDKEKF"    TO   AB-FILE.
     MOVE      JIDKEKF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
****************************************************************
*                                                              *
****************************************************************
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
*
*----<< ｼｮｷﾁ ｾｯﾄ >--*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
*----<< ｼｮｷﾒｯｾｰｼﾞ ｼｭﾂﾘｮｸ >>--*
     DISPLAY  MSG-START UPON CONS.
*
*----<<FILE OPEN >>--*
     OPEN     I-O       SHTDENF.
     OPEN     INPUT     SYUSGNF.
     OPEN     I-O       JIDKEKF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        DEN-REC.
     INITIALIZE                   DEN-REC.
*
*----<< キー情報セット >>--*
     MOVE     PARA-DATE       TO   DEN-F46.
     MOVE     PARA-TIME       TO   DEN-F47.
     MOVE     PARA-TORICD     TO   DEN-F01.

*
*----<< FILE START >>--*
     START    SHTDENF  KEY  >=   DEN-F46
                                 DEN-F47
                                 DEN-F01
                                 DEN-F48
                                 DEN-F02
                                 DEN-F04
                                 DEN-F051
                                 DEN-F07
                                 DEN-F112
                                 DEN-F03
        INVALID   KEY
           MOVE      9    TO   END-FG
             GO   TO   INIT-EXIT
     END-START.
*
*----<< １レコード目読込 >>--*
     PERFORM  900-DEN-READ.

 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*----<< 変数クリア >>--*
     MOVE      SPACE     TO      TAISHO-FLG.
*
*----<< 自動欠品結果ファイル読込 >>--*
     MOVE     DEN-F46         TO   JID-F46.
     MOVE     DEN-F47         TO   JID-F47.
     MOVE     DEN-F01         TO   JID-F01.
     MOVE     DEN-F48         TO   JID-F48.
     MOVE     DEN-F02         TO   JID-F02.
     MOVE     DEN-F04         TO   JID-F04.
     MOVE     DEN-F051        TO   JID-F051.
     MOVE     DEN-F07         TO   JID-F07.
     MOVE     DEN-F112        TO   JID-F112.
     MOVE     DEN-F03         TO   JID-F03.
     PERFORM  900-JID-READ.
*
*----<< 自動欠品結果ファイルが存在しない場合 >>--*
     IF  JID-INVALID-FLG = " "
         MOVE     DEN-REC     TO   JID-REC
     END-IF.
*
*----<< 納品日が指定された場合 >>--*
     IF  PARA-NOBI  NOT =  ZERO
       THEN
         IF  PARA-NOBI  =  DEN-F112
           PERFORM  900-ZAIKO-SUB
           MOVE     ZAIKO-HIKIATE-FLG
                             TO   DEN-F27D
           PERFORM  900-DEN-WRITE
           MOVE     "1"      TO   TAISHO-FLG
         END-IF
       ELSE
*----<< 出荷制限マスタ読込 >>--*
         MOVE     DEN-F01  TO   SYU-F01
         MOVE     DEN-F07  TO   SYU-F02
         MOVE     DEN-F25  TO   SYU-F03
         PERFORM  900-SYU-READ
*----<< 出荷制限マスタが存在した場合 >>--*
       IF  SYU-INVALID-FLG = "1"
         PERFORM  900-ZAIKO-SUB
           MOVE     ZAIKO-HIKIATE-FLG
                           TO   DEN-F27D
         PERFORM  900-DEN-WRITE
         MOVE     "1"      TO   TAISHO-FLG
       END-IF
     END-IF.
*
*----<< 自動欠品結果ファイル更新 >>--*
     IF  TAISHO-FLG  =  "1"
         MOVE     "1"      TO   JID-F70
         ADD      1        TO   WRT-CNT
     END-IF.
     IF  JID-INVALID-FLG = "1"
       THEN
         REWRITE    JID-REC
       ELSE
         WRITE      JID-REC
     END-IF.
*
*----<< 次レコード読込 >>--*
     PERFORM  900-DEN-READ.

 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
*----<<FILE CLOSE >>--*
     CLOSE     SHTDENF.
     CLOSE     SYUSGNF.
     CLOSE     JIDKEKF.
*
*****IF  OUT-CNT = ZERO
*        MOVE      4001         TO   PROGRAM-STATUS
*****END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    売上伝票ファイル READ (SEQUENTIAL)           *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
     READ     SHTDENF
       AT END
           MOVE      9    TO   END-FG
           GO  TO  900-DEN-READ-EXIT
       NOT AT END
           ADD       1    TO   RD-CNT
     END-READ.
*
     IF (DEN-F46  = PARA-DATE AND
         DEN-F47  = PARA-TIME AND
         DEN-F01  = PARA-TORICD )
     THEN
         CONTINUE
     ELSE
           MOVE      9    TO   END-FG
           GO  TO  900-DEN-READ-EXIT
     END-IF.
*
*----<< 売上計上済は読み飛ばし >>--*
     IF  DEN-F277  =  9
         GO  TO  900-DEN-READ
     END-IF.
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    売上伝票ファイル WRITE                       *
*--------------------------------------------------------------*
 900-DEN-WRITE          SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "900-DEN-WRITE"     TO   S-NAME.
*
*----<< 更新項目セット >>--*
*数量
     MOVE     ZERO                TO   DEN-F15.
*原価金額
     MOVE     ZERO                TO   DEN-F181.
*売価金額
     MOVE     ZERO                TO   DEN-F182.
*消費税
     MOVE     ZERO                TO   DEN-F19.
*粗利
     MOVE     ZERO                TO   DEN-F20.
*
*----<< 売上伝票ファイル更新 >>--*
     REWRITE    DEN-REC.
*
 900-DEN-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    出荷制限マスタ READ (RANDOM)                 *
*--------------------------------------------------------------*
 900-SYU-READ           SECTION.
     MOVE     "900-SYU-READ"      TO   S-NAME.
     READ     SYUSGNF
       INVALID
              MOVE      SPACE          TO   SYU-INVALID-FLG
       NOT INVALID
              MOVE      "1"            TO   SYU-INVALID-FLG
     END-READ.
 900-SYU-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    自動欠品結果ファイル READ (RANDOM)           *
*--------------------------------------------------------------*
 900-JID-READ           SECTION.
     MOVE     "900-JID-READ"      TO   S-NAME.
     READ     JIDKEKF
       INVALID
              MOVE      SPACE          TO   JID-INVALID-FLG
       NOT INVALID
              MOVE      "1"            TO   JID-INVALID-FLG
     END-READ.
 900-JID-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    在庫更新サブルーチン                         *
*--------------------------------------------------------------*
 900-ZAIKO-SUB          SECTION.
*
*----<< 変数クリア >>--*
     INITIALIZE                   ZAIKO-UPDATE-AREA.
*
*----<< 項目セット >>--*
     MOVE     "1"                 TO   ZAIKO-MODE.
     MOVE     DEN-F01             TO   ZAIKO-TORICD.
     MOVE     DEN-F25             TO   ZAIKO-AITESHOHINCD.
     MOVE     DEN-F08             TO   ZAIKO-SOKOCD.
     MOVE     DEN-F1411           TO   ZAIKO-SHOHINCD.
     MOVE     DEN-F1412           TO   ZAIKO-HINTANCD.
     MOVE     DEN-F112            TO   ZAIKO-NOHINBI.
     MOVE     DEN-F15             TO   HENKOMAE-SURYO.
     MOVE     ZERO                TO   HENKOGO-SURYO.
     MOVE     DEN-F49             TO   ZAIKO-TANABAN.
     MOVE     DEN-F27D            TO   ZAIKO-HIKIATE-FLG.
*
*----<< サブルーチンコール >>--*
     CALL    "SJK9010B" USING     ZAIKO-MODE
                                  ZAIKO-TORICD
                                  ZAIKO-AITESHOHINCD
                                  ZAIKO-SOKOCD
                                  ZAIKO-SHOHINCD
                                  ZAIKO-HINTANCD
                                  ZAIKO-NOHINBI
                                  HENKOMAE-SURYO
                                  HENKOGO-SURYO
                                  ZAIKO-TANABAN
                                  ZAIKO-HIKIATE-FLG.
 900-ZAIKO-SUB-EXIT.
     EXIT.
*-------------< PROGRAM END >----------------------------------*

```
