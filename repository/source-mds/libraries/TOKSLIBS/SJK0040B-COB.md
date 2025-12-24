# SJK0040B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJK0040B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品　　　　　　　　　　　*
*    モジュール名　　　　：　受信自動欠品取消処理　　　　　　　*
*    作成日／更新日　　　：　2015/10/09(FRI)                   *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*                        ：　受信時刻     PARA-TIME            *
*                        ：　納品日       PARA-NOBI            *
*    処理概要　　　　　　：　自動欠品結果Ｆを読み、売上伝票Ｆ  *
*                        ：　の出荷数を更新する。　　　　　　  *
*    更新日／更新者　　　：　2018/03/16 高橋/NAV
*    更新概要　　　　　　：　受注残Ｆ更新　　　　　　　　　　　*
*                                                              *
****************************************************************
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/03/16 高橋　　　受注残ファイル更新処理を追加　　　
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJK0040B.
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
*----<< 自動欠品結果ファイル >>--*
     SELECT   JIDKEKF   ASSIGN         DA-01-VI-JIDKEKLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
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
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
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
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 自動欠品結果ファイル >>--*
 FD  JIDKEKF            LABEL     RECORD   IS   STANDARD.
     COPY     JIDKEKF   OF        XFDLIB
              JOINING   JID       PREFIX.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  JIDKEKF-ST        PIC  X(02).
 01  SHTDENF-ST        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  DEN-INVALID-FLG   PIC  X(01).
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
     03  ZAIKO-MODE          PIC  X(01)     VALUE  "2".
     03  ZAIKO-TORICD        PIC  9(08).
     03  ZAIKO-AITESHOHINCD  PIC  X(13).
     03  ZAIKO-SOKOCD        PIC  X(02).
     03  ZAIKO-SHOHINCD      PIC  X(08).
     03  ZAIKO-HINTANCD      PIC  X(08).
     03  ZAIKO-NOHINBI       PIC  9(08).
     03  HENKOMAE-SURYO      PIC  9(09).
     03  HENKOGO-SURYO       PIC  9(09).
     03  ZAIKO-TANABAN       PIC  X(06).
     03  ZAIKO-HIKIATE-FLG   PIC  X(01).
*
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJK0040B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0040B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0040B".
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
*#2018/03/16 NAV ST
*受注残ファイル更新用ワーク
 01  PARA-AREA.
     03  PARA-KOSINKBN         PIC X(01).
     03  PARA-DTKBN            PIC X(01).
     03  PARA-TOKCD            PIC 9(08).
     03  PARA-DENNO            PIC 9(09).
     03  PARA-GYO              PIC 9(02).
     03  PARA-SOUSAI           PIC 9(01).
     03  PARA-DENKU            PIC 9(02).
     03  PARA-TENCD            PIC 9(05).
     03  PARA-SOKCD            PIC X(02).
     03  PARA-HATYU            PIC 9(08).
     03  PARA-NOUHIN           PIC 9(08).
     03  PARA-SYUKA            PIC 9(08).
     03  PARA-SAKATACD         PIC X(08).
     03  PARA-HINTAN1          PIC X(05).
     03  PARA-HINTAN2          PIC X(02).
     03  PARA-HINTAN3          PIC X(01).
     03  PARA-TANABAN          PIC X(06).
     03  PARA-JYUTYUSU         PIC 9(09).
     03  PARA-SYUKASU          PIC 9(09).
     03  PARA-GENKA            PIC 9(09).
     03  PARA-BAIKA            PIC 9(09).
     03  PARA-GENKAKIN         PIC 9(09).
     03  PARA-BAIKAKIN         PIC 9(09).
     03  PARA-AITECD           PIC X(13).
     03  PARA-HIKIATE          PIC X(01).
     03  PARA-UPBUMON          PIC X(04).
     03  PARA-UPTANCD          PIC X(02).
     03  PARA-JDATE            PIC 9(08).
     03  PARA-JTIME            PIC 9(04).
   01  PARA-BUMON              PIC X(04).
   01  PARA-TANCD              PIC X(02).
*#2018/03/15 NAV ED
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
     OPEN     I-O       JIDKEKF.
     OPEN     I-O       SHTDENF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        JID-REC.
     INITIALIZE                   JID-REC.
*
*----<< キー情報セット >>--*
     MOVE     PARA-DATE       TO   JID-F46.
     MOVE     PARA-TIME       TO   JID-F47.
     MOVE     PARA-TORICD     TO   JID-F01.

*
*----<< FILE START >>--*
     START    JIDKEKF  KEY  >=   JID-F46
                                 JID-F47
                                 JID-F01
                                 JID-F48
                                 JID-F02
                                 JID-F04
                                 JID-F051
                                 JID-F07
                                 JID-F112
                                 JID-F03
        INVALID   KEY
           MOVE      9    TO   END-FG
             GO   TO   INIT-EXIT
     END-START.
*
*----<< １レコード目読込 >>--*
     PERFORM  900-JID-READ.

 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"      TO   S-NAME.
*
*----<< 変数クリア >>--*
     MOVE     SPACE          TO      TAISHO-FLG.
*
*----<< 売上伝票ファイル読込 >>--*
     MOVE     JID-F46        TO      DEN-F46.
     MOVE     JID-F47        TO      DEN-F47.
     MOVE     JID-F01        TO      DEN-F01.
     MOVE     JID-F48        TO      DEN-F48.
     MOVE     JID-F02        TO      DEN-F02.
     MOVE     JID-F04        TO      DEN-F04.
     MOVE     JID-F051       TO      DEN-F051.
     MOVE     JID-F07        TO      DEN-F07.
     MOVE     JID-F112       TO      DEN-F112.
     MOVE     JID-F03        TO      DEN-F03.
     PERFORM  900-DEN-READ.
*
*----<< 売上伝票ファイルが存在した場合 >>--*
     IF  DEN-INVALID-FLG = "1"
       THEN
*----<< 納品日が指定された場合 >>--*
         IF  PARA-NOBI  NOT =  ZERO
           THEN
             IF  PARA-NOBI  =  JID-F112
               PERFORM  900-ZAIKO-SUB
               MOVE     ZAIKO-HIKIATE-FLG
                                 TO   DEN-F27D
               PERFORM  900-DEN-WRITE
               MOVE     "1"      TO   TAISHO-FLG
***************2018/03/16 NAV ST
               PERFORM  JYUZANF-UPD-SEC
***************2018/03/16 NAV ED
             END-IF
           ELSE
             PERFORM  900-ZAIKO-SUB
             MOVE     ZAIKO-HIKIATE-FLG
                               TO   DEN-F27D
             PERFORM  900-DEN-WRITE
             MOVE     "1"      TO   TAISHO-FLG
***************2018/03/16 NAV ST
             PERFORM  JYUZANF-UPD-SEC
***************2018/03/16 NAV ED
         END-IF
     END-IF.
*
*----<< 自動欠品結果ファイル更新 >>--*
     IF  TAISHO-FLG  =  "1"
         PERFORM  900-JID-WRITE
         ADD      1        TO   WRT-CNT
     END-IF.
*
*----<< 次レコード読込 >>--*
     PERFORM  900-JID-READ.

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
     CLOSE     JIDKEKF.
     CLOSE     SHTDENF.
*
     IF  OUT-CNT = ZERO
         MOVE      4001         TO   PROGRAM-STATUS
     END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    自動欠品結果ファイル READ (SEQUENTIAL)       *
*--------------------------------------------------------------*
 900-JID-READ           SECTION.
     MOVE     "900-JID-READ"      TO   S-NAME.
     READ     JIDKEKF
       AT END
           MOVE      9    TO   END-FG
       NOT AT END
           ADD       1    TO   RD-CNT
     END-READ.
*
     IF (JID-F46  = PARA-DATE AND
         JID-F47  = PARA-TIME AND
         JID-F01  = PARA-TORICD )
     THEN
         CONTINUE
     ELSE
           MOVE      9    TO   END-FG
     END-IF.
 900-JID-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    売上伝票ファイル READ (RANDOM)               *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
     READ     SHTDENF
       INVALID
              MOVE      SPACE          TO   DEN-INVALID-FLG
       NOT INVALID
              MOVE      "1"            TO   DEN-INVALID-FLG
     END-READ.
*
*----<< 売上計上済は処理しない >>--*
     IF  DEN-F277  =  9
              MOVE      SPACE          TO   DEN-INVALID-FLG
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
     MOVE     JID-F15             TO   DEN-F15.
*原価金額
     MOVE     JID-F181            TO   DEN-F181.
*売価金額
     MOVE     JID-F182            TO   DEN-F182.
*消費税
     MOVE     JID-F19             TO   DEN-F19.
*粗利
     MOVE     JID-F20             TO   DEN-F20.
*
*----<< 売上伝票ファイル更新 >>--*
     REWRITE    DEN-REC.
*
 900-DEN-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    自動欠品結果ファイル WRITE                   *
*--------------------------------------------------------------*
 900-JID-WRITE          SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "900-JID-WRITE"     TO   S-NAME.
*
*----<< 更新項目セット >>--*
*自動欠品ＦＬＧ
     MOVE     SPACE               TO   JID-F70.
*
*----<< 自動欠品結果ファイル更新 >>--*
     REWRITE    JID-REC.
*
 900-JID-WRITE-EXIT.
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
     MOVE     "2"                 TO   ZAIKO-MODE.
     MOVE     DEN-F01             TO   ZAIKO-TORICD.
     MOVE     DEN-F25             TO   ZAIKO-AITESHOHINCD.
     MOVE     DEN-F08             TO   ZAIKO-SOKOCD.
     MOVE     DEN-F1411           TO   ZAIKO-SHOHINCD.
     MOVE     DEN-F1412           TO   ZAIKO-HINTANCD.
     MOVE     DEN-F112            TO   ZAIKO-NOHINBI.
     MOVE     DEN-F15             TO   HENKOMAE-SURYO.
     MOVE     JID-F15             TO   HENKOGO-SURYO.
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
*--------------------------------------------------------------*
*    受注残ファイル更新処理
*--------------------------------------------------------------*
 JYUZANF-UPD-SEC        SECTION.
*
     MOVE     "JYUZANF-UPD-SEC"        TO   S-NAME.
*
     MOVE      "2"                     TO   PARA-KOSINKBN.
     MOVE      "1"                     TO   PARA-DTKBN.
     MOVE      DEN-F01                 TO   PARA-TOKCD.
     MOVE      DEN-F02                 TO   PARA-DENNO.
     MOVE      DEN-F03                 TO   PARA-GYO.
     MOVE      DEN-F04                 TO   PARA-SOUSAI.
     MOVE      DEN-F051                TO   PARA-DENKU.
     MOVE      DEN-F07                 TO   PARA-TENCD.
     MOVE      DEN-F08                 TO   PARA-SOKCD.
     MOVE      DEN-F111                TO   PARA-HATYU.
     MOVE      DEN-F112                TO   PARA-NOUHIN.
     MOVE      DEN-F113                TO   PARA-SYUKA.
     MOVE      DEN-F1411               TO   PARA-SAKATACD.
     MOVE      DEN-F1412(1:5)          TO   PARA-HINTAN1.
     MOVE      DEN-F1412(6:2)          TO   PARA-HINTAN2.
     MOVE      DEN-F1412(8:1)          TO   PARA-HINTAN3.
     MOVE      DEN-F49                 TO   PARA-TANABAN.
     MOVE      DEN-F50                 TO   PARA-JYUTYUSU.
     MOVE      DEN-F15                 TO   PARA-SYUKASU.
     MOVE      DEN-F172                TO   PARA-GENKA.
     MOVE      DEN-F173                TO   PARA-BAIKA.
     MOVE      DEN-F181                TO   PARA-GENKAKIN.
     MOVE      DEN-F182                TO   PARA-BAIKAKIN.
     MOVE      DEN-F25                 TO   PARA-AITECD.
     MOVE      ZAIKO-HIKIATE-FLG       TO   PARA-HIKIATE.
     MOVE      "2920"                  TO   PARA-UPBUMON
                                            PARA-BUMON.
     MOVE      "99"                    TO   PARA-UPTANCD
                                            PARA-TANCD.
     MOVE      DEN-F46                 TO   PARA-JDATE.
     MOVE      DEN-F47                 TO   PARA-JTIME.
*サブルーチンコール
     CALL    "SJZ0110B" USING PARA-AREA
                              PARA-BUMON
                              PARA-TANCD.
*
 JYUZANF-UPD-EXIT.
     EXIT.
*-------------< PROGRAM END >----------------------------------*

```
