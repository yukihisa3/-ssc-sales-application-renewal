****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫管理システム　　　　　　　　　*
*    モジュール名　　　　：　日次振替　　　　　　　　　　　　　*
*    作成日／更新日　　　：　00/06/20                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　電算室振替データ更新Ｆを読んで、　*
*                            在庫マスタを更新，又は追加する。　*
*                                                              *
*    94.08.02            :   富士通側締日以降に前月分データが  *
*                        :   渡された場合前月分データの更新を  *
*                        :   行う　　　　　　                  *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SFU9999B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<< 電算室振替データ更新ファイル       >>---*
     SELECT   FURIKAF   ASSIGN    TO        DA-01-VI-FURIKAL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   FUR-F01
                                                 FUR-F02
                                                 FUR-F03
                                                 FUR-F04
                                                 FUR-F05
                        FILE    STATUS      IS   FUR-STATUS.
*
*---<<  ＴＯＫＵファイル（エラー分）---*
     SELECT   FURIKAE   ASSIGN    TO        DA-01-S-FURIKAE
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   FUE-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＴＯＫＵファイル  >>---*
 FD    FURIKAF.
       COPY   FURIKAF   OF        XFDLIB
              JOINING   FUR       PREFIX.
*---<<  ＴＯＫＵファイル  >>---*
 FD    FURIKAE.
       COPY   FURIKAF   OF        XFDLIB
              JOINING   FUE       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  FUR-STATUS          PIC  X(02).
     02  FUE-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  REWRITE-CNT         PIC  9(07)  VALUE ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE ZERO.
     02  ERR-CNT             PIC  9(07)  VALUE ZERO.
****  ＷＲＫ領域  ***
 01  WRK-AREA.
     02  WRK-DATE1           PIC  9(06).
     02  WRK-DATE1R          REDEFINES   WRK-DATE1.
         04  WRK-DATE1R1     PIC  9(04).
         04  WRK-DATE1R2     PIC  9(02).
     02  WRK-DATE2           PIC  9(06).
     02  SYS-DATE            PIC  9(06)  VALUE  ZERO.
     02  SYS-DATE-YMD        PIC  9(08)  VALUE  ZERO.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SFU0120B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    FURIKAF.
     MOVE     "FURIKAF"         TO   ERR-FL-ID.
     MOVE     FUR-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 KEI0100-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 KEI0100-END.
     EXIT.
****************************************************************
*      ■０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
     OPEN     INPUT          FURIKAE
              I-O            FURIKAF.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE-YMD.
*
     PERFORM    READ-FURIKAE-SEC.
 INIT-END.
     EXIT.
****************************************************************
*      ■１　　ＴＯＫＵファイルＲＥＡＤ処理                    *
****************************************************************
 READ-FURIKAE-SEC               SECTION.
     READ    FURIKAE
         AT   END
           MOVE   "END"      TO   END-FLG
           GO                TO   READ-FURIKAE-END
     END-READ.
*
     ADD   1                 TO   READ-CNT.
 READ-FURIKAE-END.
     EXIT.
****************************************************************
*      ■０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*---((商品在庫マスタの検索キーの設定を行う))---*
     MOVE   FUE-F01          TO   FUR-F01.
     MOVE   FUE-F02          TO   FUR-F02.
     MOVE   FUE-F03          TO   FUR-F03.
     MOVE   FUE-F04          TO   FUR-F04.
     MOVE   FUE-F05          TO   FUR-F05.
     READ   FURIKAF
         INVALID   KEY
            CONTINUE
         NOT  INVALID  KEY
            ADD      1       TO   ERR-CNT
            GO               TO   MAIN-010
     END-READ.
*
     MOVE   FUE-REC   TO   FUR-REC.
     WRITE  FUR-REC.
     ADD    1         TO   WRITE-CNT.
 MAIN-010.
     PERFORM    READ-FURIKAE-SEC.
 MAIN-END.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
     CLOSE              FURIKAF  FURIKAE.
     DISPLAY  "FURIKAF    (IN) = "  READ-CNT      UPON   CONS.
     DISPLAY  "ｾｲｼﾞｮｳ          = "  WRITE-CNT     UPON   CONS.
     DISPLAY  "ｼﾞｭｳﾌｸ          = "  ERR-CNT       UPON   CONS.
 END-END.
     EXIT.
******************<<  PROGRAM  END  >>**************************
